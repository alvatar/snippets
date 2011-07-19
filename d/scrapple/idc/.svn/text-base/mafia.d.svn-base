module mafia;

import irc;
import tools.ini, tools.base, tools.functional, tools.threads;

import std.math, std.string: replace, strip, join;
Lock glock;
static this() { New(glock); }

const bool Mafia = true, Innocent = false;

mixin(ExSource);

import tools.mersenne, tools.time;
Mersenne rng;
static this() { New(rng, µsec); }

extern(C) extern nickname root_user;

mixin(genOperator("length", "return lhs.length; ", true));
mixin(genOperator("exist", "return (lhs /select/ rhs).length?true:false; "));

int strcmp(string a, string b) {
  auto shl = min(a.length, b.length);
  auto la = a[0 .. shl], lb = b[0 .. shl];
  foreach (i, ch; la) {
    if (ch < lb[i]) return -1;
    if (ch > lb[i]) return 1;
  }
  if (a.length < b.length) return -1;
  if (a.length > b.length) return 1;
  return 0;
}

struct Player {
  string name;
  bool party, dead;
}

//Threadpool tp;
//static this() { New(tp, Threadpool.GROW); }

extern(C) extern string CTRL(string channel);

void handle_mafia(Query query) {
  glock.lock();
  scope(exit) glock.unlock();
  void fail() { query.answer("Invalid query ", query.param); }
  auto file = new iniFile("mafia_state.ini");
  auto chid = query.channel.replace("#", "_"); // # is comment in .ini
  auto chq = query;
  if (chid[0] != '_') {
    if (!query.param.length || query.param[0] != '#') {
      return query.answer("Please use queries of the form \"", CTRL(query.channel), "mafia #channel <request>\".");
    }
    chq.channel = query.param.slice(" ");
    chid = chq.channel.replace("#", "_");
    auto state = file.get!(int)(chid, "state", 0);
    if (state != 2) return query.answer("Silent votes only work in the night.");
  }
  
  auto players = file.get!(Player[])(chid, "players", null);
  void savePlayers() { file.set(chid, "players", players); }
  Player[] inn() { return players /select/ ex!("x -> x.party == Innocent && !x.dead"); }
  Player[] maf() { return players /select/ ex!("x -> x.party == Mafia && !x.dead"); }
  int lookupID(string name) {
    foreach (id, player; players)
      if (player.name == name) return id;
    throw new Exception("No player by the name of "~name~"!");
  }
  Player lookup(string name) { return players[lookupID(name)]; }
  void notify(string name, string mesg) {
    auto temp = query;
    temp.name = cast(nickname) name;
    temp.notice(mesg);
  }
  bool verifyVote(string who, string whom, bool mode) {
    auto player_list = players /select/ ex!("i -> p -> p.name == i")(who);
    if (!player_list.length)
      return query.answer("You are not playing in ", chq.channel, "!"), false;
    if (player_list[0].dead)
      return query.answer("You cannot vote (due to DED)"), false;
    if (mode == Mafia)
      if (player_list[0].party != Mafia)
        return query.notice("You are not Mafia and cannot vote in the night!"), false;
    if (whom == "abstain") return true;
    if (!(players /exist/ ex!("i -> p -> p.name == i")(whom)))
      return query.answer(whom, " is not playing the game!"), false;
    if (lookup(whom).dead)
      return query.answer("You cannot vote for a dead person!"), false;
    /*if (mode == Mafia)
      if (target_list[0].party == Mafia)
        return query.answer("You are Mafia and cannot kill your own!"), false;*/
    return true;
  }
  bool checkVictory() {
    if (maf.length >= inn.length) {
      chq.say("It's over. The Mafia has won! Congratulations. ");
      return true;
    }
    if (!maf.length) {
      chq.say("It's over. The innocents have won! Congratulations. ");
      return true;
    }
    return false;
  }
  auto name = cast(string) query.name;
  /++ states:
    0: disabled
    1: gathering players
    2: main phase, night.
    3: main phase, day.
    4: completion. log result.
      transitions:
    0-1: trivial
    1-2: trivial
    2-3: timed
    3-2: timed
    2,3-4: conditional
    4-1: trivial
  ++/
  auto state = file.get!(int)(chid, "state", 0);
  void writeState(int i) {
    auto info = Format(players /select/ (Player p) { return !p.dead; } /length, " players remain. ");
    if (i == 2) chq.say("It is night. ", info, "Mafia players, please vote for a victim. ");
    if (i == 3) chq.say("It is day. ", info, "Mob, please vote for a victim. ");
    if (i == 2 || i == 3) {
      
      file.set(chid, "votes", cast(Stuple!(string, string)[]) null);
    }
    if (i == 0) {
      file.set(chid, "players", cast(Player[]) null);
      file.set(chid, "votes", cast(Stuple!(string, string)[]) null);
    }
    state = i;
    file.set(chid, "state", state);
  }
  if (query.param == "reset") {
    if (query.name != root_user) return query.answer("Not authorized!");
    writeState(0);
    return query.answer("Reset complete.");
  }
  void turn(bool timeout=false) {
    bool mode;
    if (state == 2) mode = Mafia;
    else if (state == 3) mode = Innocent;
    else assert(false);
    if (auto rest = query.param.startsWith("vote ")) {
      if (!verifyVote(name, rest, mode)) return;
      auto votes = file.get!(Stuple!(string, string)[])(chid, "votes", cast(Stuple!(string, string)[]) null);
      auto selfvoted = votes /select/ (string s, string t) { return s == name; };
      if (selfvoted.length)
        return query.answer("You already voted for ", selfvoted[0]._1);
      if (rest == "abstain") votes ~= stuple(name, cast(string) null);
      else votes ~= stuple(name, rest);
      file.set(chid, "votes", votes);
      
      int left;
      if (mode == Mafia) left = maf().length - votes.length;
      else left = (maf().length + inn().length) - votes.length;
      if (rest == "abstain") query.answer("You abstained. ", left, " votes left. ");
      else query.answer("You voted for ", rest, ". ", left, " votes left. ");
      if (!left) {
        int[string] tally;
        foreach (st; votes) if (st._1) tally[st._1] ++;
        auto field = tally
          /map/ (string k, int v) { return stuple(k, v); }
          /qsort/ (Stuple!(string, int) s1, Stuple!(string, int) s2) {
            if (s1._1 < s2._1) return -1;
            else if (s1._1 > s2._1) return 1;
            else return 0;
          };
        if (!field.length) {
          chq.say("Every player abstained this round! The game ends in a draw. ");
          writeState(0);
          return;
        }
        assert(field[0]._1 <= field[$-1]._1);
        auto kill = field[$-1]._0;
        if (mode == Mafia) {
          chq.say("Player ", kill, " has been killed in a horrible accident! Turns out, he was ",
            (lookup(kill).party == Mafia)?"Mafia":"innocent", ". There are ", maf().length, " Mafia left. ");
        } else {
          chq.say("Player ", kill, " has been lynched! Turns out, he was ",
            (lookup(kill).party == Mafia)?"Mafia":"innocent", ".");
        }
        players[lookupID(kill)].dead = true; savePlayers;
        
        if (checkVictory) {
          writeState(0);
          return;
        }
        writeState((mode == Mafia)?3:2);
      }
    } else return query.answer("Please use the form \"", CTRL(query.channel), "mafia vote <victim>\".");
  }
  switch (state) {
    case 0:
      if (query.param != "start") {
        query.answer("Mafia Proof of Concept. Use \"start\" to begin. ");
        return;
      }
      writeState(1);
      auto ctrl = CTRL(query.channel);
      chq.say("Starting new game. Use \"", ctrl, "mafia play\" to participate, \"", ctrl, "mafia start\" to start the round.");
      chq.say("In the night phase, Mafia players should participate anonymously by using privmsgs of the form \"", ctrl, "mafia #channel vote <nick>\".");
      // fallthrough to gathering
      query.param = "play";
    case 1:
      if (query.param == "play") {
        foreach (player; players) {
          if (player.name == name)
            return query.answer("You are already playing!");
        }
        players ~= Player(name, Innocent, false); savePlayers;
        query.answer("You have been added to the list of players. There are now ", players.length, " players: ",
          players /map/ ex!("p -> p.name")
        );
        return;
      }
      if (query.param == "start") {
        if (players.length < 4) return query.answer("Must have at least four players to start the game.");
        chq.say("Starting the game. Players are being notified .. ");
        foreach_reverse (id, ref player; players)
          if (id) swap(player, players[rng() % id]); // shuffle
        auto mc = players.length / 3; // a third are mafia
        auto ic = players.length - mc;
        for (int i = 0; i < mc; ++i) players[i].party = Mafia;
        // re-sort alphabetically to not give hints about shuffling
        players = players /qsort/ (Player p1, Player p2) { return strcmp(p1.name, p2.name); };
        savePlayers;
        
        foreach (player; players) {
          if (player.party == Innocent) {
            notify(player.name, Format("You are innocent. There are ", maf.length, " Mafia among you."));
          } else {
            notify(player.name, Format("You are Mafia. The list of Mafia players is: ", (maf /map/ ex!("p -> p.name")).join(", ")));
          }
        }
        writeState(2);
        break;
      }
      return fail;
    case 2: // night
    case 3: turn; break;
  }
}
