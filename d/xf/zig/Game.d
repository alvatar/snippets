module xf.zig.Game;

private {
	import xf.game.Event : Wish;
	import xf.game.GameInterface;
	import xf.game.Misc;
	import xf.game.TimeHub;
	import xf.game.TickTracker;
	
	import xf.net.NetObj : NetObjBase, NetObjObserver;
	import xf.net.ControlEvents : DestroyObject, TuneClientTiming;
	
	import xf.zig.Events;
	import xf.zig.Ship;
	import xf.zig.Beacon;
	import xf.zig.Camera;
	import xf.zig.Renderer;
	import xf.zig.GameObj;
	import xf.zig.Phys;
	import xf.zig.Projectile;
	import xf.zig.Level;
	
	import xf.core.JobHub;
	
	import xf.maths.Vec : vec2;
	import xf.maths.Misc;

	import xf.utils.Array : remove;
	import xf.utils.HardwareTimer : hardwareTimer;
	import xf.utils.Memory;
	
	import tango.math.random.Kiss : Random = Kiss;
	
	extern (C) int printf(char* format, ...);
}

public {
	import xf.game.Misc;
}



class Game(bool serverSide) : TickTracker, NetObjObserver {
	this(GameInterface gameInterface) {
		this.gameInterface = gameInterface;
		
		static if (serverSide) {
			LoginRequest.addHandler(&handle);
			initInput();
		}
		else {
			PlayerLogin.addHandler(&handle);
			LoginAccepted.addHandler(&handle);
			CreateBeacon.addHandler(&handle);
			CreateProjectile.addHandler(&handle);
		}
		
		CreateLocalProjectile.addHandler(&handle);
		DestroyProjectile.addHandler(&handle);
		CreateExplosion.addHandler(&handle);
		PlayerLogout.addHandler(&handle);
		
		timeHub.addTracker(this);
		gameInterface.addObserver(this);
		
		phys.addCollisionHandler(&this.collisionHandler);
	}
	
	
	void setLevel(char[] path) {
		this.level = new Level(path);
	}
	
	
	void start() {
		static if (serverSide) {
			/+for (int i = 0; i < 10; ++i) {
				createBeacon(gameInterface.genObjId);
			}+/
		}
	}
	
	
	private Beacon createBeacon(objId id) {
		auto b = new Beacon;
		gameInterface.register(b, id);
		
		static if (serverSide) {
			float frand() {
				uint r = Random.shared.natural();
				return (cast(float)r / uint.max - 0.5f) * 2;
			}
			
			b.centerPos = vec2[frand, frand] * 45.f;
			b.rotSpeed = (frand() + 3.f) * 0.01;
			
			jobHub.addRepeatableJob(&b.spin, 50.f);
		}
		return b;
	}
	
	
	private void initInput() {
		InputWish.addHandler(&handle);
		ShieldFocusWish.addHandler(&handle);
	}
	
	
	bool nickConnected(char[] nick) {
		foreach (pid, data; players) {
			if (nick == data.nick) return true;
		}
		return false;
	}
	
	
	bool playerLoggedIn(playerId id) {
		return (id in players) !is null;
	}
	
	
	void handle(LoginRequest e) {
		if (nickConnected(e.nick)) {
			LoginRejected(`nickname in use`).filter((playerId pid) { return pid == e.wishOrigin; }).immediate;
		} else {
			printf(`Login request accepted ! *****************************`\n);
			
			objId shipId = gameInterface.genObjId();
			createPlayerData(e.wishOrigin, e.nick, shipId);
			
			LoginAccepted(e.wishOrigin, e.nick, shipId).filter((playerId pid) { return pid == e.wishOrigin; }).immediate;
			foreach (rpid, data; players) {
				if (rpid is e.wishOrigin) continue;
				PlayerLogin(rpid, data.nick, (cast(NetObjBase)data.ship).netObjId).filter((playerId pid) { return pid == e.wishOrigin; }).immediate;
			}

			PlayerLogin(e.wishOrigin, e.nick, shipId).filter((playerId pid) { return pid != e.wishOrigin; }).immediate;
			
			foreach (b; beacons) {
				CreateBeacon(b.netObjId).filter((playerId pid) { return pid == e.wishOrigin; }).immediate;
			}
		}
	}
	
	
	void handle(LoginAccepted e) {
		printf(`Login accepted :D`\n);
		
		// bind local wishes to the local player
		Wish.defaultWishOrigin = e.pid;

		initInput();

		Camera cam = new Camera;
		with (createPlayerData(e.pid, e.nick, e.shipId)) {
			cam.follow(ship);
			ship.netObjPredicted = true;
		}
		renderer.camera = cam;
	}
	
	
	void handle(PlayerLogin e) {
		createPlayerData(e.pid, e.nick, e.shipId);
	}


	void handle(InputWish e) {
		debug if (e.action) printf(`handling input from %d meant for tick %d (at tick %d)`\n, cast(int)e.wishOrigin, e.eventTargetTick, timeHub.currentTick);
		auto player = players[e.wishOrigin];
		
		static if (serverSide) {
			tick targetTick = e.eventTargetTick;
			tick curTick = timeHub.currentTick;
			const uint desiredOffsetMillis = 30;
			
			auto offset = (curTick > targetTick) ? 	// the event should have arrived this many ticks earlier
				cast(float)(curTick - targetTick) + timeHub.secondsToTicks(desiredOffsetMillis * 0.001f)
			: {
				assert (curTick == targetTick);
				uint waitingTime = cast(uint)(hardwareTimer.timeMicros / 1000) - e.receptionTimeMillis;
				return cast(float)timeHub.secondsToTicks((cast(float)desiredOffsetMillis - waitingTime) * 0.001f);
			}();
			
			debug printf(`Wish should've arrived %f ticks %s`\n, offset > 0 ? offset : -offset, offset > 0 ? `earlier`.ptr : `later`.ptr);
			TuneClientTiming(e.wishOrigin, offset).immediate;
		}
		
		float accel = 0.015f;
		float strafeAccel = 0.007f;
		float rotAccel = 0.1f;
		
		float b2f(byte b) {
			return cast(float)b / 127.f;
		}

		player.ship.accelerate(vec2[strafeAccel * b2f(e.strafe), accel * b2f(e.thrust)]);
		player.ship.rotate(rotAccel * b2f(e.rot));
		
		if (e.action & e.Shoot) {
			CreateLocalProjectile(player.ship.position, player.ship.direction, e.wishOrigin).immediate;
			
			if (serverSide) {
				CreateProjectile(player.ship.position, player.ship.direction, e.wishOrigin).filter((playerId pid) { return pid != e.wishOrigin; }).immediate;
			}
		}
	}
	
	
	void handle(ShieldFocusWish e) {
		players[e.wishOrigin].ship.shield.focusAtAngle(e.angle8 * 45.f);
	}
	
	
	void handle(CreateBeacon e) {
		createBeacon(e.id);
	}
	
	
	ulong hashProjectile(vec2 pos, vec2 vel, playerId pid, tick creationTick) {
		ulong res = creationTick;
		res <<= 8;
		res += pid;
		res <<= 12;
		res |= cast(ulong)(((atan2(vel.x, vel.y) + pi/2) / pi) * (1 << 11));
		res <<= 8;
		res |= cast(ulong)(((atan2(vel.x, vel.y) + pi/2) / pi) * (1 << 7));
		res <<= 4;
		res |= cast(ulong)pos.len & 0b1111;
		return res;
	}
	
	
	void handle(CreateProjectile e) {
		projectiles ~= new Projectile(e.position + e.velocity * (timeHub.currentTick - e.eventTargetTick), e.velocity,
													hashProjectile(e.position, e.velocity, e.owner, e.eventTargetTick),
													150 - cast(int)(timeHub.currentTick - e.eventTargetTick));
	}
	

	void handle(CreateLocalProjectile e) {
		e.createdHash = hashProjectile(e.position, e.velocity, e.owner, e.eventTargetTick);
		e.destroyProjectileDg = &this.destroyProjectile;
		projectiles ~= new Projectile(e.position, e.velocity, e.createdHash, 150);
	}
	
	
	void handle(DestroyProjectile e) {
		destroyProjectile(e.projectileHash);
	}

	
	void handle(CreateExplosion e) {
		createExplosion(e.center, e.radius, e.strength);
	}
	
	
	void handle(PlayerLogout e) {
		printf(`* Player disconnecting`\n);

		assert (e.pid in players);
		auto player = players[e.pid];
		
		destroyGameObj(player.ship);
		players.remove(e.pid);
	}
	
	
	// ----
	
	
	private void destroyGameObj(GameObj o) {
		if (serverSide) {
			DestroyObject(o.netObjId).immediate;
			o.netObjScheduleForDeletion();
		}
	}
	
	
	private void destroyProjectile(ulong hash) {
		printf(`Destroying a projectile %llu ... `\n, hash);
		if (auto proj = getProjectileByHash(hash)) {
			destroyProjectile(proj);
		} else {
			printf(`Hrm... not found :/`\n);
		}
	}


	private void destroyProjectile(Projectile proj) {
		projectiles.remove(proj);
		proj.dispose();
	}


	private Projectile getProjectileByHash(ulong hash) {
		foreach (p; projectiles) {
			if (p.projectileHash is hash) return p;
		}
		return null;
	}
	
	
	private PlayerData createPlayerData(playerId pid_, char[] nick_, objId shipId) {
		with (players[pid_] = new PlayerData) {
			nick = nick_;
			gameInterface.register(ship = new Ship, shipId);
		}
		
		return players[pid_];
	}
	
	
	float relevanceCalcFunc(playerId pid, NetObjBase no) {
		auto go = cast(GameObj)no;
		assert (go !is null);

		auto player = players[pid];
		
		if (player.ship is go) return 0.2f;		// predicted anyway :P
		
		vec2 playerPos = player.ship.position;
		vec2 objPos = go.position;
		
		float dist = (playerPos - objPos).len;
		float res =  1.f / (1.f + dist);
		if (res !>= 0.001f) return 0.001f;
		
		if (cast(Ship)go !is null) res *= 2.f;		// all objects are equal, but some are more equal than others
		
		return res;
	}

	
	void collisionHandler(Body bv1, Body bv2) {
		auto o1 = cast(Object)bv1.userData;
		auto o2 = cast(Object)bv2.userData;
		assert (o1 !is null);
		assert (o2 !is null);
		
		debug printf(`Collision between %.*s and %.*s`\n, o1.classinfo.name, o2.classinfo.name);

		static if (serverSide) {
			if (extractCollision!(Beacon, Projectile)(bv1, bv2, &beaconCollisionHandler)) return;
		}

		if (extractCollision!(Projectile, Ship)(bv1, bv2, &projShipCollisionHandler)) return;
	}
	
	
	void beaconCollisionHandler(Beacon b, Projectile p) {
		printf(`* Destroying a beacon`\n);
		destroyGameObj(b);
		DestroyProjectile(p.projectileHash).immediate;
	}


	void projShipCollisionHandler(Projectile p, Ship s) {
		if (p.lifeRemaining > 0) {
			float damage = 40.f;
			s.hit(damage, p.position);
			p.lifeRemaining = 0;
		}
	}
	
	// ----
	
	/+static final class Explosion {
		vec2	center() {
			return bVol.origin;
		}
		
		float	strength;
		float	radius;
		BVol	bVol;
		
		this(vec2 center, float radius, float strength) {
			this.radius = radius;
			this.strength = strength;
			bVol = new BSphere(radius);
			bVol.userData = cast(void*)this;
			bVol.origin = center;
		}
	}+/
	
	
	private void createExplosion(vec2 center, float radius, float strength) {
		/+BVol bv1 = (new Explosion(center, radius, strength)).bVol;
		phys.calcCollisions(bv1, (BVol bv2) {
			extractCollision!(Explosion, GameObj)(bv1, bv2, &explosionCollisionHandler);
		});+/
	}
	

	/+private void explosionCollisionHandler(Explosion expl, GameObj obj) {
		if (cast(Projectile)obj) return;

		vec2 explCenter = expl.center;
		vec2 objCenter = obj.position;
		
		float dist = (explCenter - objCenter).len;
		if (dist < expl.radius) {
			float force = expl.strength * cos(dist / expl.radius);
			vec2 v = objCenter - explCenter;
			
			float damage = 500.f;
			if (v != vec2.zero) {
				damage /= v.len;
				v *= force / v.len;
			}
			
			obj.accelerate(v);
			
			static if (serverSide) {
				if (auto ship = cast(Ship)obj) {
					ship.hit(damage, explCenter);
				}
			}
		}
	}+/


	// TickTracker interface ----


	protected void advanceTick(uint ticks) {
		foreach (pid, data; players) {
			data.ship.update();
		}
		
		phys.update();
		
		Projectile[] remList; uint len;  // TODO: free list / pool alloc
		foreach (p; projectiles) {
			p.update();
			--p.lifeRemaining;
			if (p.lifeRemaining <= 0) {
				remList.append(p, &len);
			}
		}
		foreach (p; remList[0..len]) {
			destroyProjectile(p);
		}
		remList.free();
		
		foreach (b; beacons) {
			b.update();
		}
	}
	
	protected void trimHistory(uint ticks) {
	}
	
	protected void rollback(uint ticks) {
	}

	// ----

	void	onNetObjCreated(NetObjBase o_) {
		if (auto o = cast(Beacon)o_) {
			if (o.netObjRef) {
				beacons ~= o;
			}
		}
	}
	
	void	onNetObjDestroyed(NetObjBase o_) {
		if (auto o = cast(Beacon)o_) {
			static if (serverSide) {
				CreateExplosion(o.position, 25.f, 0.3f).delayed(.2f);
			}
			beacons.remove(o);
			static if (serverSide) jobHub.removeRepeatableJob(&o.spin);
			o.netObjUnref();
		}
	}

	// ----
	
	private {
		PlayerData[playerId]	players;
		Projectile[]				projectiles;
		GameInterface			gameInterface;
		
		Beacon[]					beacons;
		
		Level						level;
	}
}



private class PlayerData {
	char[]	nick;
	Ship		ship;
}
