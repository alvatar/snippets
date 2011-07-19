module maid;

import tools.base, tools.functional, irc, dice: rand;

// returns randomized indices of maxcount top values
int[] SelectTopRand(int[] input, int count) {
  if (!input.length) return null;
  if (input.length == 1) return [0];
  int max = input[1], maxpos;
  foreach (i, v; input) if (v > max) { max = v; maxpos = i; }
  int[] positions;
  foreach (i, v; input) if (v == max) positions ~= i;
  int[] res;
  for (int x = 0; x < count; ++x) {
    auto i = rand() % positions.length;
    res ~= positions[i];
  }
  return res;
}

// MAID RPG
struct Maid {
  enum Attributes { Athletics, Affection, Skill, Cunning, Luck, Will }
  int[6] attributes, mod;
  int[2] types;
  int uniform, uniform_alt, eye, hair;
  int roots, stress;
  string[4] special;
  struct Power { string Attrib; int id; }
  Power[] powers;
  int weapon;
  string toString() {
    string res = "Attributes: [";
    foreach (i, name; AttrNames) {
      if (i) res ~= ", ";
      res ~= Format(name, ": ", attributes[i]);
      auto mv = mod[i];
      if (!mv) continue;
      string op = " + ";
      if (mv < 0) op = " - ";
      res ~= Format(op, abs(mv), " = ", max(0, attributes[i] + mv));
    }
    res ~= "], a "~bonuses[types[0]][0]~" "~bonuses[types[1]][0];
    res ~= " maid wearing a "~colors[uniform]~" and "~colors[uniform_alt]~" uniform, "~colors[eye]~" eyes and "~colors[hair]~" hair. ";
    res ~= "\nSpecial Qualities: ";
    if (special[0] == special[1]) {
      if (special[2].length)
        res ~= Format(special[0], " [", special[2], ", ", special[3], "]");
      else res ~= special[0];
    } else {
      if (special[2].length) res ~= Format(special[0], " [", special[2], "]");
      else res ~= special[0];
      res ~= ", ";
      if (special[3].length) res ~= Format(special[1], " [", special[3], "]");
      else res ~= special[1];
    }
    res ~= Format(". Maid roots: [", Roots[roots], "]. Stress reaction: [", Stress[stress], "]. ");
    res ~= Format("Powers: ");
    foreach (i, pow; powers) {
      if (i) res ~= ", ";
      res ~= Format(pow.Attrib, "[", Powers[pow.Attrib][pow.id], "]");
    }
    res ~= ". ";
    
    auto wpn = Weapons[weapon];
    if (auto name = wpn.endsWith("*")) res ~= "She wields a "~name~". ";
    else res ~= "She fights with "~wpn~". ";
    return res;
  }
  static {
    Maid opCall() {
      Maid res;
      with (res) {
        foreach (ref attribute; attributes) attribute = ((rand() % 12) + 1) / 3;
        foreach (ref type; types) {
          type = rand() % 6;
          mod[AttrToID[bonuses[type][1]]] ++;
          mod[AttrToID[bonuses[type][2]]] --;
        }
        int d6() { return rand() % 6; }
        int d66idx() { return d6() * 6 + d6(); }
        uniform = d66idx(), uniform_alt = d66idx(), eye = d66idx(), hair = d66idx();
        special[0] = specials[rand() % $].strip();
        special[1] = specials[rand() % $].strip();
        if (auto tp = special[0] in tables) special[2] = (*tp)[rand() % $].strip();
        if (auto tp = special[1] in tables) special[3] = (*tp)[rand() % $].strip();
        roots = rand() % Roots.length;
        stress = rand() % Stress.length;
        int sum; foreach (i, attr; attributes) sum += max(0, attr + mod[i]);
        int numpowers = 1;
        if (sum <= 9) numpowers ++;
        bool[Power] dup_prevent;
        int[6] modded_attrs = attributes;
        foreach (i, ref v; modded_attrs) v += mod[i];
        foreach (id; SelectTopRand(modded_attrs, numpowers)) {
          retry:
          auto power = Power(AttrNames[id], rand() % 6);
          if (power in dup_prevent) goto retry;
          powers ~= power; dup_prevent[power] = true;
        }
        weapon = rand() % Weapons.length;
      }
      return res;
    }
    string[6] AttrNames;
    int[string] AttrToID;
    string[3][] bonuses; // name, increase, decrease
    string[] colors, specials;
    string[][string] tables; // special tables
    string[] Roots, Stress;
    string[][string] Powers;
    string[] Weapons;
    static this() {
      AttrNames[] = ["Athletics"[], "Affection", "Skill", "Cunning", "Luck", "Will"];
      foreach (i, name; AttrNames) AttrToID[name] = i;
      // bonuses["Lolita"][] = [Bonus(Attributes.Luck, 1), Bonus(Attributes.Athletics, -1)];
      // seems a bit verbose, no?
      mixin(ReplaceConcat!(3, `bonuses ~= ["NAME", "ATT1", "ATT2"]; `,
        "NAME",    "ATT1",      "ATT2",
        "Lolita",  "Luck",      "Athletics",
        "Sexy",    "Cunning",   "Will",
        "Pure",    "Affection", "Cunning",
        "Cool",    "Skill",     "Affection",
        "Boyish",  "Athletics", "Skill",
        "Heroine", "Will",      "Luck"
      ));
      colors = "
        Red,        Purple, Orange, Pink,   Brown,    Vermillion,
        Purple,     Blue,   Green,  Sky,    Navy,     Indigo,
        Orange,     Green,  Yellow, Cream,  Beige,    Gold,
        Pink,       Sky,    Cream,  White,  Gray,     Silver,
        Brown,      Navy,   Beige,  Gray,   Black,    Metallic,
        Vermillion, Indigo, Gold,   Silver, Metallic, Transparent/Rainbow".split(",") /map/ &strip;
      assert(colors.length == 6 * 6);
      specials = "
        Glasses, Freckles, Sickly, Quiet, Easygoing, Neat Freak,
        Brown Skin, Albino, Shy, Trap, Imaginative, Greedy,
        Elf Ears, Nekomimi, Android, Vampire, Princess, Angel, Devil,
        
        Uniform, Symbol, Delinquent, Accent, Hairstyle, Accessory,
        Relationship, Perversion, Criminal Tendencies, Injury, Tragic Love, Dark Past, Trauma,
        Secret Job, Membership, Shapeshifter, Monster, Magic, Absurd".split(",");
      mixin(ReplaceConcat!(2, `tables["NAME"] = "TEXT".split(",") /map/ &strip; `, "NAME", "TEXT",
        "Uniform", "Tights, China Dress, Armor, Bondage, Miniskirt, Kappougi",
        "Symbol", "Skull, Bat, Cross, Yin-Yang, Star, Card Suit",
        "Delinquent", "Cigarettes, Tattoo, Sunglasses, Bad Expression, Piercings, Rough Speak",
        "Accent", "Southern, British, Pidgin English, Meow, Knight, Foreigner",
        "Hairstyle", "Long Ringlets, Dumplings, Mesh, Curly Hair, Emo Hair, Antenna Hair",
        "Accessory", "Collar, Large Ribbon, Spike, Chains, Black Leather Gloves, Pet",
        
        "Relationship", "Sibling, Childhood Friends, Mentor, Friendly Rival, Ex-Lover, Love Rival, Vengeance",
        "Perversion", "Nymphomaniac, Sadist, Masochist, Womanizer, Likes Them Young, Exhibitionist",
        "Criminal Tendencies", "Killer, Pyromaniac, Kleptomaniac, Addict, Otaku, Stalker",
        "Injury", "Patchwork, One Eye, Burns, Whip Scars, Bandages, Blind",
        "Tragic Love", "Separations, Lover Died, Killed Your Lover, Former Prostitute, Betrayal, Stalker Damage",
        "Dark Past", "Former Delinquent, Former Killer, Amnesiac, Bad Reputation, Wanted, Runaway",
        "Trauma", "Suicide Attempts, Killed Your Parents, Saw Parent Die, Sibling Hate, Family Breakup, Abusive Parents",
        
        "Secret Job", "Assassin, Hacker, Scientist, Doctor/Pharmacist, Doujin Artist, Pro Creator",
        "Membership", "Evil Secret Society, Secret Agency, Cult, Political Organization, Shadow Clan, Government Official",
        "Shapeshifter", "Fox, Spider, Raven, Bunny, Tiger/Lion, Snake",
        "Monster", "Mermaid, Zombie/Mummy, Werewolf, Succubus, Ghost, Shinigami",
        "Magic", "Priestess, Onmyouji, Fortune Teller, Western Magician, Devil Summoner, Necromancer",
        "Absurd", "Alien, Cyborg, Runaway Ninja, Magical Girl, Fairy, Mutant"
      ));
      Roots = "Family Debts, Slave, Master's Mistress, Revenge, Orphan, Illegitimate Child, Hereditary Maid,
        Self-Punishment, Unrequited Love, Business, Infiltrator, Loyalty, Childhood Friend, Admirer of Maids,
        Returning a Favor, Distant Relative, Bridal Training, Who knows?".split(",") /map/ &strip;
      Stress = "Alcohol/Drugs, Stealing, Violence, Gambling, Racing, Teasing, Mischief, Running Away, Complaining,
        Seclusion, Crying, Rampage, Shopping, Sleep, Eating Binge, Prayer, Spoiled Child, Player Choice".split(",") /map/ &strip;
      mixin(ReplaceConcat!(2, `Powers["NAME"] = "TEXT".split(",") /map/ &strip; `, "NAME", "TEXT",
        "Athletics", "Super Evasion, Iron Wall, Trespass, Weapon From Nowhere, Giant Weapon, Ultimate Retort",
        "Affection", "Maiden's Tears, World for Two, Power of Friendship, Cooked With Love, Windows of the Soul, Passionate Gaze",
        "Skill", "Lock Picking, Stalking, Lie Detector, Ultimate Menu, Instant Cleaning, Hammerspace Dress",
        "Cunning", "Punishment, Instant Restraint, Coercion, Trap, Fake Crying, Mockery",
        "Luck", "Karma, Saw It, Teleport, Escape, Foreboding, Chance Meeting",
        "Will", "Immune to Pain, Crisis Adrenaline, Persistence, Tenacity, Hard Work, Absolute Maid"
      ));
      Weapons = ("Mop/Broom, Stun Gun, Kitchen Knife, Frying Pan, Vase/Bottle/Pot,
        Revolver, Machinegun, Rifle, Bomb/Grenade, Bazooka, Ray Gun, Metal Pipe/Nail Bat, Hammer,
        Scythe, Kung Fu Weapon, Chainsaw, Wooden Sword/Staff, Axe/Hatchet, Morningstar,
        Western Sword, Whip, Spear/Lance, Exotic Weapon, Knife/Scalpel, Chain/Rope, Katana, Halberd/Pole Arm, Book".split(",")
        /map/ (string s) { return s.strip() ~ "*"; }
        )~"Claws, Hand-to-Hand,
        Shuriken/Kunai, Summoning, Magic, Psychic Powers,
        Internal Weapons, Religious Symbol".split(",") /map/ &strip;
    }
  }
}

void mkMaid(Query query) {
  auto maid = Maid();
  query.answer(maid.toString());
}

struct Mansion {
  string era; int subtype;
  int mood, color;
  int[] special;
  string toString() {
    string res = Format("It's a ", Maid.colors[color], " ", Appearance[era][subtype], " from the ", era, " era. ");
    res ~= "Its mood is "~Mood[mood]~". ";
    res ~= "It ";
    bool wasHas = false;
    foreach (i, v; special) {
      if (i) {
        if (i == special.length - 1) res ~= " and ";
        else res ~= ", ";
      }
      string prefix = "has a ";
      auto text = Special[v];
      if (auto rest = text.endsWith("[1]")) { prefix = "has an "; text = rest; } else
      if (auto rest = text.endsWith("[]")) { prefix = "has "; text = rest; } else
      if (auto rest = text.endsWith("[2]")) { prefix = "is "; text = rest; }
      if (auto rest = prefix.startsWith("has ")) {
        if (wasHas) prefix = rest;
        else wasHas = true;
      }
      res ~= prefix~text;
    }
    res ~= ".";
    return res;
  }
  static {
    Mansion opCall() {
      Mansion res;
      with (res) {
        era = Appearance.keys[rand() % $];
        subtype = rand() % Appearance[era].length;
        mood = rand() % Mood.length;
        color = rand() % Maid.colors.length;
        for (int i = 0; i < 2; ++i) {
          res.special ~= rand() % Special.length;
        }
      }
      return res;
    }
    string[][string] Appearance;
    string[] Mood;
    // [1]: an  []: no prefix [2]: state
    string[] Special;
    static this() {
      Mood = "Romance, Light, Action, Hard, Dark, Horror".split(", ");
      mixin(ReplaceConcat!(2, `Appearance["NAME"] = "TEXT".split(",") /map/ &strip; `, "NAME", "TEXT",
        "Fantasy", "Castle, Palace, House, Ruins, Tower, Dungeon",
        "Early Modern", "Castle, Palatial Residence, House, Ruins, Bar, Military Installation",
        "Contemporary", "Business, Palatial Residence, House, Ruins, Room, Laboratory",
        "Space", "Warship, Extravagant Spaceship, Colony, Second-Hand Spaceship, Private Planet, Artificial Planet",
        "Cyber", "Palatial Residence, Business, Office, Ruins, Room, Corporate",
        "Post-Apocalyptic", "Castle, Underground Facility, Fortress, Ruins, Tower, Prison",
        "Wild Kingdom", "Tree-house, Fortress, Huts, House, Ruins, Palatial Residence",
        "Old West", "Palatial Residence, Saloon, Casino, Church, Ranch, The Open Road",
        "Old Edo", "Castle, ninja Village, Samurai Residence, Tenement, Red Light District, Shinto Shrine"
      ));
      Special = "Beach, Large Bath, Open-Air Bath[1], Indoor Pool[1], Outdoor Pool[1], Tennis Court,
        Perfect Shielding[], Soundproof[2], Under Construction[2], Mobile[2], Barrier,
        Electromagnetic Isolation[], Game Room, Medical Room, Religious Facility, Secret Annex,
        Canopied Bed, Traces of Memories[], Self-Destruct Switch, Strategic Preparations[],
        Treasury, Stable, Jungle, Private Army, Superintendent, Secret Base, Waste Disposal Facility,
        Underground Kingdom[1], Secret Passages[], Portrait of the Master, Laboratory, Otherworld Gate[1],
        Hidden Room, Torture Room, Old Well[1], Dungeon".split(",") /map/ &strip;
    }
  }
}

void mkMansion(Query query) {
  auto mansion = Mansion();
  query.answer(mansion.toString());
}
