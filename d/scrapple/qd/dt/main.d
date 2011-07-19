module dt.main; // D Tower Defense

import tools.base;
import std.perf;
import qd, tools.vector, tools.ca_rng, tools.compat, tools.time;
alias tools.ca_rng.rand rand;

import tools.time;
bool delegate() blink(double delay) {
  return stuple(delay, true, sec()) /apply/ (double delay, ref bool state, ref double last) {
    if (sec() < last) {
      last = sec(); // timer wrapped around. Can't help this. Don't switch. Hope the user doesn't notice. Should be raaaaare.
      return state;
    }
    while (sec() - last > delay) { last += delay; state = !state; }
    return state;
  };
}

float randf() { return (cast(float)rand())/(cast(float)(typeof(rand()).max)); }

rgb Background;
static this() { Background=Yellow~White; }

class Thing {
  abstract void draw();
  abstract void update();
}

interface InteractingThing {
  void selected(bool b); void highlighted(bool b); void broken(bool b);
  bool isSelected(); bool isHighlighted(); bool isBroken();
}

template InteractingImpl() {
  private bool select, highlight, breaks;
  void selected(bool b) {
    select = b;
    if (b) highlight = true;
  }
  void highlighted(bool b) { highlight = b; }
  void broken(bool b) { breaks = b; }
  bool isSelected() { return select; }
  bool isHighlighted() { return select || highlight; }
  bool isBroken() { return breaks; }
}

class WorldThing : Thing {
  vec2f pos;
  int radius;
  bool collides(WorldThing other) {
    return (pos-other.pos).length <= radius + other.radius;
  }
  mixin This!("pos, radius");
  bool collide(WorldThing other) { return distance(other)<radius+other.radius; }
  float distance(WorldThing other) { return (other.pos - pos).length(); }
}

class LivingThing : WorldThing {
  private {
    int life, maxlife, age;
    void delegate(LivingThing) onDeath;
  }
  override void update() { age++; }
  void damage(int howmuch) { life-=howmuch; if (life!>0) onDeath(this); }
  void die() { damage(life); onDeath(this); }
  mixin This!("onDeath, super(pos, radius), life=10; #maxlife=life; ");
}

interface MovingThing {
  vec2f getMotionVector();
}

class Path {
  vec2f[] points;
  bool loop;
  this(bool loop, vec2f[] points...) { this.loop=loop; this.points=points.dup; }
  class Train {
    private {
      size_t cur;
      float speed;
      vec2f pos;
      void delegate() done;
    }
    vec2f delta;
    mixin This!("pos, speed, done, cur=0");
    void update() {
      if (cur!<points.length) throw new Exception("The train has arrived!");
      float len;
      // find the next point that's more than 1 unit away from me
      do {
        delta = points[cur] - pos;
        len = delta.length();
      } while (len!>1 && (++cur)<points.length);
      if (cur==points.length) {
        if (loop) cur=0; else done();
        return;
      }
      delta /= len/speed;
      pos += delta;
    }
  }
}

struct PreRenderedArea {
  Area area;
  void blit(int x, int y) {
    display.select(area.size, pt(x, y), Center).blit(area);
  }
}

PreRenderedArea renderCircle(T...)(int radius, T t) {
  PreRenderedArea res;
  screen.With(null, {
    screen(radius*2+2, radius*2+2, 0, true);
    flip=false;
    //SDL_SetAlpha(display, SDL_SRCALPHA, 255);
    uint back=255;//SDL_MapRGBA(display.format, 0, 0, 0, 255);
    SDL_SetColorKey(display.surface, SDL_SRCCOLORKEY, back);
    SDL_FillRect(display.surface, null, back);
    circle(radius+1, radius+1, radius, t);
    res.area=display;
  });
  return res;
}

PreRenderedArea[string] circleBuffer;
void bufferedCircle(T...)(int x, int y, int radius, T t) {
  circle(x, y, radius, t);
  return;
  /*string key=
    (cast(char*)&x)[0..int.sizeof] ~
    (cast(char*)&y)[0..int.sizeof];
  foreach (e; t) key~=(cast(char*)&e)[0..typeof(e).sizeof];
  if (!(key in circleBuffer)) circleBuffer[key]=renderCircle(radius, t);
  circleBuffer[key].blit(x, y);*/
}

class Critter : LivingThing , MovingThing {
  void draw() {
    bufferedCircle(pos.to!(int).tuple, radius, Black, Fill=Blue~White~White);
    if (life<maxlife) circle(pos.to!(int).tuple, cast(int)(radius*0.8*(1f-life * 1f / maxlife)), Background~Red, Fill=Background~Red);
    string fmt;
    if (life>1000) fmt = Format("[size -5]", life, "[/size]");
    else if (life > 100) fmt = Format("[size -3]", life, "[/size]");
    else fmt = Format(life);
    fmt = "[color #040]"~fmt~"[/color]";
    .prettyprint(pos.to!(int).tuple, Center, fmt);
  }
  override vec2f getMotionVector() { return train.delta; }
  void damage(typeof(life) d) {
    super.damage(d);
    if (life!>0) {
      addEffect!(RingEffect)(White~Yellow, pos, 10, 15);
      addEffect!(RingEffect)(Black, pos, 11, 15);
    }
  }
  void update() {
    train.update;
    pos = train.pos;
  }
  float speed;
  void delegate() win;
  void done() { win(); die(); }
  private {
    Path path;
    Path.Train train;
    void initTrain() {
      train=path.new Train(pos, speed, &done);
    }
    long id;
  }
  mixin This!("path, speed, win, id, super(onDeath, pos, radius=20, life=20), #initTrain; ");
}

class Ammo : LivingThing {
  TargetGroup targets;
  vec2f delta;
  // float length, angle;
  struct Settings {
    float speed, aim, speed_momentum, dir_momentum;
    int damage, lifetime;
    bool linger, punchthrough, retarget;
  }
  Settings settings;
  mixin This!("settings, targets, super(onDeath, pos, radius=3, life=10)
    #life=settings.damage; #delta=vec2f(0f); "); // #angle = 0f;
  LivingThing enemy;
  void draw() {
    //line(x, y, enemy.x, enemy.y, White);
    auto color = Black.blend(White, age * 1f / settings.lifetime);
    bufferedCircle(pos.to!(int).tuple, radius, Fill=color, color);
  }
  void damage(typeof(life) d) {
    int prelife = life;
    super.damage(d);
    if (life!>0) addEffect!(RingEffect)(Red, pos, 3, cast(int) (log(prelife) + PI_2)); // I WANNA PIE TOO
  }
  void hit(LivingThing target) {
    typeof(life) dmg;
    if (settings.punchthrough) dmg=min(life, target.life);
    else dmg=life;
    target.damage(dmg); damage(dmg);
  }
  void update() {
    if (!(pos in display)) die();
    super.update();
    if (age > settings.lifetime) die();
    if (enemy && enemy.life!>0) enemy=null; // This enemy is ded.
    if (!enemy) {
      if (settings.retarget) {
        enemy=targets.getTarget(this);
      }
      if (!enemy && !settings.linger) { die(); return; }
    }
    vec2f new_motion;
    /*vec2f delta() {
      auto res = vec2f(1, 0);
      res.rotate(angle); res.length = length;
      return res;
    }*/
    if (enemy) {
      new_motion = (enemy.pos - pos).normalized() * (1f - settings.aim);
      if (auto mt=cast(MovingThing) enemy) {
        new_motion += mt.getMotionVector().normalized() * settings.aim;
      }
      new_motion.length = settings.speed;
      
      auto old_motion = delta;
      auto length = delta.length;
      auto m = settings.dir_momentum;
      // delta = old_motion.rotate(old_motion.angle(new_motion) * m);
      delta = old_motion * m + new_motion * (1f - m);
      // logln("δ: ", delta, " (*", m, ")");
      
      delta.length = length * settings.speed_momentum + new_motion.length * (1f - settings.speed_momentum);
      if (isnan(delta.length)) {
        delta = new_motion;
        delta.length = 0.01;
      }
    }
    pos += delta;
    if (enemy && collide(enemy)) { hit(enemy); return; }
    // if we accidentally hit one of the other targets, we settle for that.
    foreach (target; targets) if (target && collide(target)) hit(target);
  }
}

void remove_ref(T)(ref T[] objects, Object which) {
  size_t pos=size_t.max;
  foreach (idx, obj; objects) if (obj is which) { pos=idx; break; }
  if (pos==size_t.max)
    //throw new Exception("Reference not found");
    return;
  if (objects.length>1) objects[pos]=objects[$-1];
  objects=objects[0..$-1];
  remove_ref(objects, which);
}

bool has_ref(T)(T[] objects, T which) {
  foreach (obj; objects) if (obj is which) return true;
  return false;
}

class Group(T) : Thing {
  static assert(is(T: Thing), "Group must be of Things");
  T[] things;
  this(T[] things...) { this.things=things; }
  void addThing(T[] shiny...) { things~=shiny; } // SHINY THINGS ^_______^
  void draw() { foreach (thing; things) if (thing) thing.draw; }
  void update() { foreach (thing; things) if (thing) thing.update; things.remove_ref(cast(Object)null); }
  void remove(T which) { foreach (ref thing; things) if (which is thing) thing=null; }
  void removeAs(U)(U which) { foreach (ref thing; things) if (which is thing) thing=null; }
  size_t length() { return things.length; }
  int opApply(int delegate(ref T Critter) dg) { 
    foreach (ref thing; things) {
      auto res=dg(thing); if (res) return res;
    }
    return 0;
  }
}

class TargetGroup : Group!(Critter) {
  void delegate()[][Critter] onDeaths; // WOO HOO
  Critter getTarget(WorldThing whoAsks, float maxrange=float.max, void delegate() dies=null) {
    Critter res;
    auto lowest_id=ulong.max;
    foreach (thing; things) if (thing) {
      auto newdist=thing.distance(whoAsks);
      //if (newdist<dist && newdist-thing.radius<maxrange) {
      // try to find the critter with the lowest ID; i.e. the first
      if (thing.id<lowest_id && newdist-thing.radius<maxrange) {
        lowest_id=thing.id;
        res=thing;
      }
    }
    if (!res) return null;
    if (!(res in onDeaths)) onDeaths[res]=null;
    if (dies && !onDeaths[res].has_ref(dies)) onDeaths[res]~=dies;
    return res;
  }
  bool live() { return things.length?true:false; }
  void update() {
    super.update();
    foreach (ref feep; things) if (feep.life!>0) if (feep in onDeaths) {
      foreach (DeathNote; onDeaths[feep]) DeathNote();
      onDeaths.remove(feep);
      feep=null;
    }
    things.remove_ref(cast(Object)null);
  }
}

class Turret : WorldThing, InteractingThing {
  mixin InteractingImpl!();
  struct Settings {
    int range, rate;
    bool kickstart=false;
  }
  Settings settings;
  Ammo.Settings rsettings;
  private {
    int counter;
    TargetGroup targets;
  }
  Group!(Ammo) Teh_Ordnance;
  mixin This!("settings, rsettings, targets, super(pos, radius=12),
    #counter=settings.rate > 0 ? settings.rate : 1; #Teh_Ordnance=new Group!(Ammo);");
  void draw() {
    auto ipos = pos.to!(int);
    if (isHighlighted) {
      circle(ipos.tuple, settings.range+1, White);
      circle(ipos.tuple, settings.range, Black);
      circle(ipos.tuple, settings.range-1, White);
    }
    rgb col;
    if (isSelected) col = White~Green~(Blue~White);
    else if (isBroken) col = White~Red;
    else col = White~Green;
    bufferedCircle(ipos.tuple, radius, Black, Fill=col);
    Teh_Ordnance.draw;
  }
  private void die(LivingThing not_so_living_now_are_you) {
    Teh_Ordnance.remove(cast(Ammo)cast(void*)not_so_living_now_are_you); // trust me. this is correct.
  }
  void update() {
    scope(exit) Teh_Ordnance.update;
    auto enemy=targets.getTarget(this, settings.range);
    if (!enemy) return;
    if (counter-->0) return;
    counter = settings.rate > 0 ? settings.rate : 1;
    int times = 1;
    if (settings.rate < 0) times = abs(settings.rate);
    Range[times].each = {
      auto newThing=new Ammo(rsettings, targets, &die, pos);
      if (settings.kickstart) {
        newThing.delta = vec2f.rand();
      }
      newThing.enemy=enemy;
      Teh_Ordnance.addThing(newThing); newThing.update;
    };
  }
}

void withSet(T)(ref T var, T val, void delegate() dg) {
  auto backup = var; var = val;
  scope(exit) var = backup;
  dg();
}

import SDL_ttf;
class SpawnPoint : WorldThing {
  private {
    int rate, counter, groupsize, group_left, breaktime, break_left;
    float life;
    bool break_mode;
    Path path;
    int score;
    void delegate() critter_hit;
  }
  TargetGroup critters;
  mixin This!("rate, groupsize, breaktime, life, critter_hit, path, super(pos, radius=15), #counter=0;
    #group_left=groupsize; #critters=new TargetGroup; #break_mode=true; #break_left=breaktime; ");
  void draw() {
    string text;
    if (break_mode)
      if (critters.live) text = "..";
      else if (break_left) text = Format("-", break_left);
      else text = Format("GO");
    else if (group_left) text = Format(group_left);
    critters.draw;
    withSet(alpha, 64, {
      circle(pos.to!(int).tuple, radius, Fill=Background~Black);
    });
    .print(pos.to!(int).tuple, Center, Black, text);
  }
  void win() { critter_hit(); }
  static long id=0;
  void update() {
    critters.update;
    if (counter > 0) { counter --; return; }
    else counter = rate;
    if (break_mode) {
      if (critters.live) return;
      else if (!break_left)
        break_mode = false;
      else { break_left --; return; }
    }
    if (!group_left) {
      group_left = groupsize;
      if (breaktime) {
        break_mode = true;
        break_left = breaktime;
      }
      life = life * 1.1 + 1;
    } else group_left --;
    // yes the last critter is intentionally stronger.
    auto newCritter=new Critter(path, 0.5, &win, id++,
      stuple(critters, this) /apply/ (TargetGroup tg, typeof(this) This, LivingThing lt) { tg.remove(cast(Critter) lt); This.score += lt.maxlife; },
      pos, 10, cast(int) life);
    critters.addThing(newCritter); newCritter.update;
  }
}

const float pi180=PI/180f;

class RingEffect : LivingThing {
  float count;
  rgb color;
  mixin This!("color, super(onDeath, pos, radius=3, life=8) #count=cast(float)0; ");
  void draw() {
    auto ipos = pos.to!(int);
    circle(ipos.tuple, cast(int)(radius+count+1), color~Yellow);
    circle(ipos.tuple, cast(int)(radius+count), color);
    const int lines=6;
    for (int i=0; i<360; i+=360/lines) {
      line(
        cast(int)(pos.x+sin(i*pi180)*(radius+count)), cast(int)(pos.y+cos(i*pi180)*(radius+count)),
        cast(int)(pos.x+sin(i*pi180)*(radius+count+4)), cast(int)(pos.y+cos(i*pi180)*(radius+count+4)),
        color);
    }
  }
  void update() { count+=randf*randf; if (count>life) damage(life); }
}

Group!(LivingThing) effects;
static this() {
  effects=new typeof(effects);
}
struct addEffect(T) {
  static void opCall(U...)(U u) {
    effects.addThing(new T(u[0], &effects.removeAs!(LivingThing), u[1..$]));
  }
}

class FrameLimiter {
  int fps, rps;
  mixin This!("fps, rps, #ms=fps?(1000/fps):0; #rms=rps?(1000/rps):0; ");
  private {
    uint count, rcount;
    int ms, rms;
  }
  void limit(void delegate() flip) {
    auto cur=SDL_GetTicks();
    if (!rps) { flip(); return; }
    if (cur-rcount>ms) {
      rcount=cur;
      auto pc=new PerformanceCounter;
      pc.start;
      flip();
      pc.stop;
      //writefln("Flipped - took ", pc.milliseconds, "ms!");
      cur=SDL_GetTicks();
    }
    scope(exit) count=cur;
    if (fps && (cur-count<ms)) {
      //writefln("Delay ", ms-(cur-count));
      SDL_Delay(ms-(cur-count));
    } //else writefln("No delay - ", (cur-count)-ms, " over!");
  }
}

class InteractionSet {
  alias InteractingThing IT;
  bool[IT] selected, highlighted;
  void cleanHighlights() { foreach (entry; highlighted.keys) entry.highlighted = false; highlighted = null; }
  void cleanSelection() { foreach (entry; selected.keys) entry.selected = false; selected = null; }
  void rmHighlight(IT it) { it.selected = false; highlighted.remove(it); }
  void addHighlight(IT it) { highlighted[it] = true; it.highlighted = true; }
  void mouse_up() {
    if (!(SDLKey.LCtrl in keystate)) cleanSelection;
    foreach (entry; highlighted.keys) {
      entry.selected = true;
      selected[entry] = true;
    }
    cleanHighlights();
  }
}

import dt.fatline, tools.functional;
void main() {
  auto blink_fn = blink(0.5);
  vec2f mouse_pos, start;
  bool drag=false;
  auto thingies = new InteractionSet;
  bool underMouse(WorldThing wt) {
    with (wt) return (pos - mouse_pos).length() <= radius;
  }
  bool underSelection(WorldThing wt) {
    logln(cast(void*) wt, " under selection?");
    with (wt) {
      if (pos.x+radius<min(start.x, mouse_pos.x)) return false;
      if (pos.x-radius>max(start.x, mouse_pos.x)) return false;
      if (pos.y+radius<min(start.y, mouse_pos.y)) return false;
      if (pos.y-radius>max(start.y, mouse_pos.y)) return false;
    }
    logln(cast(void*) wt, " under selection.");
    return true;
  }
  screen(640, 480);
  while (!mouse.clicked) { flip; events; }
  cls(Background);
  Group!(Thing) group;
  auto path=new Path(false,
    vec2f(60, 60), vec2f(60, 300), vec2f(200, 300), vec2f(200, 30),
      vec2f(440, 30), vec2f(440, 150), vec2f(300, 150), vec2f(300, 300), vec2f(500, 300)
  );
  FatLine fl;
  auto bg_surf = screen.With(screen.w, screen.h) = {
    fl = new FatLine(path.points, (float d) {
      if (abs(d) > fl.width) return Background;
      else if (abs(d) > fl.width - 2) return Background.blend(Black, (fl.width - abs(d)) / 2f);
      else if (abs(d) > fl.width - 4) return Black.blend(White, (fl.width - 2 - abs(d)) / 2f);
      else return White;
    }, 18);
    fl.render();
  };
  
  int hits_counter = 10;
  void hit() {
    hits_counter --;
    if (!hits_counter) throw new Exception("You lose!");
  }
  auto spawner=new SpawnPoint(/* rate */ 48, /* groupsize */ 10, /* breaktime */ 5, /* start life */ 64, &hit, path, path.points[0]);
  group=new typeof(group)([cast(Thing)spawner]);
  group.addThing(new Turret(
    Turret.Settings(115, 24, false),
    Ammo.Settings(/*speed*/ 2f, /*aim*/ 0.25, /*momentum_speed*/0.995, /*momentum_dir*/ 0.99, /*damage*/ 8, /*lifetime*/512, /*linger*/true, /*punchthrough*/false),
    spawner.critters, vec2f(30, 340)
  ));
  group.addThing(new Turret(
    Turret.Settings(115, 12, false),
    Ammo.Settings(/*speed*/ 0.9f, /*aim*/ 0, /*momentum_speed*/0.9, /*momentum_dir*/ 0.98, /*damage*/ 2, /*lifetime*/1024, true, false, true),
    spawner.critters, vec2f(360, 85)
  ));
  group.addThing(effects);
  flip=false;
  int fps;
  auto now = sec();
  float avg=0; int count=0;
  scope(exit) writefln("Average FPS: ", avg/(cast(float)count));
  auto limiter=new FrameLimiter(60, 60);
  bool delegate(WorldThing) shouldHighlight = &underMouse;
  Turret test_turret;
  void reset_test_turret() {
    test_turret = new Turret(
      Turret.Settings(115, /*12*/ -4, true),
      Ammo.Settings(0.5, 0.2, 0.9, 0.9, 1, 1024, true, false, false),
      spawner.critters, vec2f(0, 0)
    );
  }
  reset_test_turret;
  bool turret_clicked;
  do with (group) {
    update;
    // addEffect!(RingEffect)(Blue~Black, mousex, mousey, 50, 50);
    test_turret.pos = vec2f(mouse.pos.x, mouse.pos.y);
    display.blit(bg_surf);
    prettyprint(0, 0, Bottom|Right,
      Format("Score: [b][size -2]", spawner.score, "[/size][/b]"));
    draw;
    test_turret.broken = fl.circle_collides(test_turret.pos, test_turret.radius) || {
      foreach (t; group)
        if (auto meep = cast(WorldThing) t)
          if (meep.collides(test_turret)) return true;
      return false;
    }();
    
    if (turret_clicked) test_turret.draw();
    
    auto foo = display.select(pt(100, 40), Bottom|Left);
    line(foo.tl, foo.br, Box=Black, Fill=Background~White);
    .print(foo, Center, Black, "Place turret");
    
    turret_clicked |= mouse.released && mouse.pos in foo;
    
    withSet(alpha, 64, { circle(path.points[$-1].to!(int).tuple, 15, Fill=Yellow~Black); });
    string formatting = Format(hits_counter);
    auto b = blink_fn();
    if (hits_counter <= 3) formatting = "[size +4]"~(b?"[color #f88][b]":"")~formatting~(b?"[/b][/color]":"")~"[/size]";
    formatting = "[color #c80]"~formatting~"[/color]";
    .prettyprint(path.points[$-1].to!(int).tuple, Center, formatting);
    if (drag) {
      tintfill(start.min(mouse_pos).to!(int).tuple, start.max(mouse_pos).to!(int).tuple, Blue~White);
      line(start.to!(int).tuple, mouse_pos.to!(int).tuple, Box=White);
    }
    limiter.limit({ flip; });
    events;
    if (key.pressed(SDLKey.Escape)) return;
    mouse_pos.x = mouse.pos.x; mouse_pos.y = mouse.pos.y;
    if (mouse.moved) {
      foreach (thing; group) {
        auto wt = cast(WorldThing) thing, it = cast(InteractingThing) thing;
        if (wt && it)
          if (shouldHighlight(wt)) thingies.addHighlight(it);
          else thingies.rmHighlight(it);
      }
    }
    if (!turret_clicked && mouse.clicked) {
      start.x = mouse.pos.x; start.y = mouse.pos.y;
      drag = true;
      shouldHighlight = &underSelection;
    }
    if (drag && mouse.released) {
      logln("RELEASED");
      thingies.mouse_up;
      shouldHighlight = &underMouse;
      drag = false;
    }
    
    if (turret_clicked && !test_turret.isBroken && mouse.clicked) {
      turret_clicked = false;
      group.addThing(test_turret);
      reset_test_turret;
    }
    
    ++fps;
    if (now + 1 < sec()) {
      now = sec();
      writefln(fps, " FPS");
      ulong rcount=0;
      foreach (entry; group) if (auto t=cast(Turret) entry) rcount+=t.Teh_Ordnance.length;
      writefln(rcount, " explody thingies");
      avg+=fps; ++count;
      fps=0;
    }
  } while (true);
}
