module loli;

import irc;

import tools.ini, tools.base, tools.functional;

import std.math, std.string: replace, strip;

import tools.vector;

struct WHERESTR(string EX) { static bool opCall(T)(T ·) { return mixin(ctReplace(EX, "$", "·")); } }
template WHERE(string EX) { const WHERESTR!(EX) WHERE; }

int cmp(T)(T a, T b) { if (a == b) return 0; if (a < b) return -1; return 1; }

mixin(ExSource); // so that ex!() can access local functions

template Tag(string NAME) {
  mixin("typedef bool _"~NAME~"; ");
  mixin("const _"~NAME~" "~NAME~" = true; ");
}

mixin Tag!("severely");

mixin(genOperator("ptrs", "ElemType!(LHS)*[] res; foreach (ref entry; lhs) res ~= &entry; return res; ", true));

const Stepsize = 0.4;
struct Loli {
  struct FightResult {
    int damage;
    int killed;
    FightResult opCatAssign(FightResult fr) { damage += fr.damage; killed += fr.killed; return *this; }
  }
  int health, maxhealth;
  bool warrior;
  string inTransit; // to whom
  vec2f pos;
  int attack() {
    if (!warrior) return 1;
    return injured(severely)?6:injured()?2:4;
  }
  int defense() {
    if (!warrior) return 1;
    return injured(severely)?2:injured()?3:4;
  }
  bool injured(_severely sev=false) { return health <= maxhealth * (sev?0.5:0.3); }
  bool step(vec2f target) {
    auto dir = target - pos;
    if (dir.length < Stepsize) {
      pos = target;
      return true;
    } else {
      dir.length = Stepsize;
      pos += dir;
      return false;
    }
  }
  static Loli opCall(bool warrior) {
    Loli res; res.warrior = warrior;
    res.maxhealth = res.health = 10;
    return res;
  }
  FightResult fight(ref Loli target) {
    auto att = attack(), def = target.defense();
    auto dmg = def - att;
    if (dmg < 0) return FightResult(0, false);
    target.health = max(0, target.health - dmg);
    if (!target.health) return FightResult(dmg, true);
    return FightResult(dmg, false);
  }
  FightResult fight(UserState state) {
    auto defenders = state.lolis /ptrs /select/ WHERE!("!$.inTransit && $.warrior") /qsort/ ex!("a, b -> cmp(a.health, b.health)");
    auto res = FightResult(0, 0);
    if (defenders.length) {
      res ~= fight(*defenders[0]);
    }
    return res;
  }
}

import tools.mersenne: Mersenne;

Mersenne rng;
static this() { New(rng); rng.seedFrom(maingen); }

int randrange(int from, int to) { return (rng() % (to-from+1)) + from; }
float randf() { return rng.randf(); }

import std.date;

mixin(genOperator("count", "return lhs.length; ", true));
mixin(genOperator("sum", "int res; foreach (entry; lhs) res += entry; return res; ", true));

struct WTF(T) { static T opCall() { throw new Exception("WTF?"); } }

long secs() { return getUTCtime() / TicksPerSecond; }

string plural(int count, string word, string case1 = null, string case0 = null) {
  if (!count)
    if (case0) return case0;
    else return Format("no ", word, "s");
  if (count == 1)
    if (case1) return case1;
    else Format("one ", word);
  if (case0 && case1) return Format(count, word);
  else return Format(count, " ", word, "s");
}

struct UserState {
  // lastplayed indicates when the user last made a move
  // lastchecked indicates when the internal state was last updated
  long lastplayed, lastchecked;
  vec2f pos;
  Loli[] lolis;
  static UserState rand(int players) {
    auto count = randrange(3, 7);
    UserState res;
    res.lastplayed = res.lastchecked = secs();
    for (int i = 0; i < count; ++i)
      res.lolis ~= res.spawn();
    res.pos = vec2f.rand_sphere * (sqrt(cast(real) players) + 3);
    return res;
  }
  int warriors() { return lolis /select/ WHERE!("$.warrior") /count; }
  string[] messages;
  void update(Query query, UserState delegate(string id) dg) { // does not affect lastChecked
    auto time = secs();
    long delta() { return time - lastchecked; }
    while (delta > minute) {
      Loli.FightResult fres, counter_res;
      void delegate() response;
      foreach (ref loli; lolis) {
        if (loli.inTransit) {
          auto target = dg(loli.inTransit);
          if (loli.step(target.pos)) {
            fres ~= loli.fight(target);
          }
        } else loli.health = min(loli.maxhealth, loli.health + 1);
      }
      lastchecked += minute;
    }
  }
  Loli spawn() {
    auto f = randf();
    if (f < 0.16) return Loli(true);
    return Loli(false);
  }
  string infos() {
    return info(lolis.length, warriors);
  }
  static string info(int all, int w) {
    if (!all) return "no lolis.";
    auto res = plural(all, "loli");
    if (w) {
      res ~= Format(", of which ", plural(w, " are warriors", "one is a warrior", "none are warriors"));
    }
    res ~= ".";
    return res;
  }
}

UserState genState(Query query, int users) {
  auto state = UserState.rand(users);
  query.answer("Hi, and welcome to the Loli Alternate Game! I'll start you out with ", state.infos());
  query.answer("Your settlement is located at <", state.pos.x, ", ", state.pos.y, ">");
  return state;
}

const second = 1, minute = 60 * second, hour = 60 * minute, day = 24 * hour, week = 7 * day, year = 365 * day;

extern(C) extern nickname root_user;

void loliHandle(Query query) {
  auto file = new iniFile("loli_state.ini");
  if (query.channel == "#anime") {
    query.answer("Can't play, luke-jr will kick me :(");
    return;
  }
  auto id = cast(string) query.name ~ "@" ~ query.channel.replace("#", "_"); // # == comment
  if (query.param == "reset") {
    if (query.name == root_user) {
      file.del("users", "");
      query.answer("Game reset");
    } else query.answer("Not authorized!");
    return;
  }
  if (query.param.length && !file.has("users", id)) {
    return query.answer("You are not playing!");
  }
  bool started;
  auto state = file.get!(UserState)("users", id, ((started = true), genState(query, file.section("users").length)));
  scope(exit) file.set("users", id, state);
  if (started) return;
  if (auto rest = query.param.startsWith("status").strip()) {
    if (!rest.length) return query.answer("You have "~state.infos());
    auto other_id = rest~"@"~query.channel.replace("#", "_");
    if (!file.has("users", other_id))
      return query.answer(rest~" is not playing on "~query.channel);
    return query.answer(rest~" has "~file.get!(UserState)("users", other_id, WTF!(UserState)()).infos());
  }
  if (query.param.length) {
    return query.answer("Unknown command "~query.param);
  }
  auto time = secs();
  int delta() { return time - state.lastplayed; }
  if (delta > 24 * hour) state.lastplayed = time - 24 * hour;
  int warriors, total;
  while (delta > minute) {
    state.lastplayed += minute;
    auto f = randf();
    if (f >= 0.004) continue;
    auto nl = state.spawn();
    if (nl.warrior) warriors ++;
    total ++;
    state.lolis ~= nl;
  }
  if (!total) return query.answer("No changes!");
  return query.answer("You have gained "~UserState.info(total, warriors));
}
