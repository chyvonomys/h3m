extern crate flate2;
extern crate colored;
use colored::Colorize;

#[macro_use]
extern crate nom;

use std::fs::File;
use std::io::{BufReader, Read};
use flate2::bufread::GzDecoder;

use nom::{le_u8 as eat_8, le_u16 as eat_16, le_u32 as eat_32};

macro_rules! eat_option (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        switch!($i,
            eat_8,
            1u8 => map!($submac!($($args)*), |x| Some(x)) |
            0u8 => value!(None)
        )
    );
    ($i:expr, $f:expr) => (
        eat_option!($i, call!($f))
    );
);

macro_rules! versions (
    ($i:expr, $v:expr, $roe:ident!( $($roe_args:tt)* ), $ab:ident!( $($ab_args:tt)* ), $sod:ident!( $($sod_args:tt)* )) => (
        switch!($i, value!($v),
            H3MVersion::RoE => $roe!($($roe_args)*) |
            H3MVersion::AB => $ab!($($ab_args)*) |
            H3MVersion::SoD => $sod!($($sod_args)*)
        )
    );
);

macro_rules! h3m_enum {
    ( <$t:ident, $f:ident, $p:ident, $d:ty> ($i0:expr, $x0:ident, $o0:expr) $( ($i:expr, $x:ident, $o:expr) )* ) => (
        #[derive(Debug)]
        enum $t {
            $x0 $( , $x )*
        }

        named!($f<$t>, switch!($p,
            $i0 => value!($t::$x0) $( | $i => value!($t::$x) )*
        ));

        impl $t {
            fn to_debug(&self) -> $d {
                match *self {
                    $t::$x0 => $o0 $( , $t::$x => $o )*
                }
            }
        }
    );
    ( <$t:ident, $f:ident, $p:ident> ($i0:expr, $x0:ident) $( ($i:expr, $x:ident) )* ) => (
        #[derive(Debug)]
        enum $t {
            $x0 $( , $x )*
        }

        named!($f<$t>, switch!($p,
            $i0 => value!($t::$x0) $( | $i => value!($t::$x) )*
        ));
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MVersion, eat_version, eat_32>
    (0x0000000E, RoE)
    (0x00000015, AB)
    (0x0000001C, SoD)
}

impl Clone for H3MVersion {
    fn clone(&self) -> Self { *self }
}
impl Copy for H3MVersion {}

///////////////////////////////////////////////////////////////////////////////////////////////////

named!(eat_flag<bool>, switch!(eat_8,
    0 => value!(false) |
    1 => value!(true)
));

named!(eat_string<String>, do_parse!(
    n: eat_32 >>
    st: take!(n) >>
    (String::from_utf8(Vec::from(st)).unwrap_or(String::from("<bad utf8>")))
));

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MSize, eat_size, eat_32, usize>
    (36, S, 36)
    (72, M, 72)
    (108, L, 108)
    (144, XL, 144)
}

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MDifficulty, eat_difficulty, eat_8>
    (0, Easy)
    (1, Normal)
    (2, Hard)
    (3, Expert)
    (4, Impossible)
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MHeader {
    version: H3MVersion,
    has_players: bool,
    size: H3MSize,
    has_underground: bool,
    name: String,
    description: String,
    difficulty: H3MDifficulty,
    level_cap: u8, // AB/SoD
}

impl H3MHeader {
    fn get_width(&self) -> usize {
        self.size.to_debug()
    }
    fn get_height(&self) -> usize {
        self.size.to_debug()
    }
}

named!(eat_header<H3MHeader>, do_parse!(
    version: eat_version >>
    has_players: eat_flag >>
    size: eat_size >>
    has_underground: eat_flag >>
    name: eat_string >>
    description: eat_string >>
    difficulty: eat_difficulty >>
    level_cap: versions!(version, value!(0u8), call!(eat_8), call!(eat_8)) >>
    (H3MHeader {
        version,
        has_players,
        size,
        has_underground,
        name,
        description,
        difficulty,
        level_cap,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MTownKind, eat_town_kind, eat_8>
    (0x00, Castle)
    (0x01, Rampart)
    (0x02, Tower)
    (0x03, Inferno)
    (0x04, Necropolis)
    (0x05, Dungeon)
    (0x06, Stronghold)
    (0x07, Fortress)
    (0x08, Conflux)
    (0xFF, Random)
}

///////////////////////////////////////////////////////////////////////////////////////////////////

struct H3MLocation(u8, u8, bool);

named!(eat_location<H3MLocation>, do_parse!(
    x: eat_8 >> y: eat_8 >> u: eat_flag >>
    (H3MLocation(x, y, u))
));

impl std::fmt::Debug for H3MLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{} {}{}>", self.0, self.1, if self.2 { " U" } else { "" })
    }
}

struct H3MSpellsMask(u32, u32, u8);

named!(eat_spells_mask<H3MSpellsMask>,
       map!(tuple!(eat_32, eat_32, eat_8), |t| H3MSpellsMask(t.0, t.1, t.2))
);

impl Default for H3MSpellsMask {
    fn default() -> Self {
        H3MSpellsMask(0u32, 0u32, 0u8)
    }
}

impl std::fmt::Debug for H3MSpellsMask {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{:08b} {:032b} {:032b}>", self.2, self.1, self.0) // first bit last
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MMainTown {
    generate_hero: bool, // AB/SoD
    kind: H3MTownKind, // AB/SoD, TODO: check what this field means
    location: H3MLocation,
}

named_args!(eat_main_town(version: H3MVersion)<H3MMainTown>, do_parse!(
    generate_hero: versions!(version, value!(true), call!(eat_flag), call!(eat_flag)) >>
    kind: versions!(version, value!(H3MTownKind::Random), call!(eat_town_kind), call!(eat_town_kind)) >>
    location: eat_location >>
    (H3MMainTown {
        generate_hero, kind, location,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MPlayerBehavior, eat_player_behavior, eat_8>
    (0, Random)
    (1, Warrior)
    (2, Builder)
    (3, Explorer)
}

///////////////////////////////////////////////////////////////////////////////////////////////////

struct H3MResources([u32; 7]);

named!(eat_resources<H3MResources>, map!(count_fixed!(u32, eat_32, 7), |xs| H3MResources(xs)));

impl std::fmt::Debug for H3MResources {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<W:{} M:{} O:{} S:{} C:{} G:{} $:{}>",
               self.0[0], self.0[1], self.0[2], self.0[3], self.0[4], self.0[5], self.0[6]
        )
    }
}

#[derive(Debug)]
struct H3MArtifact(u16);

named!(eat_artifact1<H3MArtifact>, map!(eat_8, |i| H3MArtifact(i as u16)));
named!(eat_artifact2<H3MArtifact>, map!(eat_16, |i| H3MArtifact(i)));

named_args!(eat_artifact(version: H3MVersion)<H3MArtifact>,
    versions!(version, call!(eat_artifact1), call!(eat_artifact2), call!(eat_artifact2))
);

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MResource, eat_resource, eat_8>
    (0, Wood)
    (1, Mercury)
    (2, Ore)
    (3, Sulfur)
    (4, Crystals)
    (5, Gems)
    (6, Gold)
}

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MHallLevel, eat_hall_level, eat_8>
    (0, Town)
    (1, City)
    (2, Capitol)
}

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MCastleLevel, eat_castle_level, eat_8>
    (0, Fort)
    (1, Citadel)
    (2, Castle)
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy)]
struct H3MCreature(u16);

named!(eat_creature1<H3MCreature>, map!(eat_8, |i| H3MCreature(i as u16)));
named!(eat_creature2<H3MCreature>, map!(eat_16, |i| H3MCreature(i)));

named_args!(eat_creature(version: H3MVersion)<H3MCreature>,
    versions!(version, call!(eat_creature1), call!(eat_creature2), call!(eat_creature2))
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MSpecialVictoryCondition {
    condition: H3MVictoryCondition,
    or_default: bool,
    cpu_allowed: bool,
}

named_args!(eat_special_victory(version: H3MVersion)<Option<H3MSpecialVictoryCondition>>,
    switch!(peek!(eat_8),
        0xFF => value!(None, eat_8) |
        _ => do_parse!(
            code: eat_8 >>
            or_default: eat_flag >>
            cpu_allowed: eat_flag >>
            condition: call!(eat_victory, version, code) >>
            (Some(H3MSpecialVictoryCondition {
                condition, or_default, cpu_allowed
            }))
        )
    )
);

#[derive(Debug)]
enum H3MVictoryCondition {
    AcquireArtifact(H3MArtifact),
    AccumCreatures(H3MCreature, u32),
    AccumResources(H3MResource, u32),
    UpgradeTown(H3MLocation, H3MHallLevel, H3MCastleLevel),
    BuildGrail(Option<H3MLocation>),
    DefeatHero(H3MLocation),
    CaptureTown(H3MLocation),
    DefeatMonster(H3MLocation),
    FlagAllDwellings,
    FlagAllMines,
    TransportArtifact(H3MArtifact, H3MLocation), // NOTE: all versions artifact v1 here
}

named_args!(eat_victory(version: H3MVersion, code: u8)<H3MVictoryCondition>, switch!(value!(code),
    0x00 => map!(call!(eat_artifact, version), |art| H3MVictoryCondition::AcquireArtifact(art)) |
    0x01 => do_parse!(cr: call!(eat_creature, version) >> amount: eat_32 >>
                      (H3MVictoryCondition::AccumCreatures(cr, amount))) |
    0x02 => do_parse!(res: eat_resource >> amount: eat_32 >>
                      (H3MVictoryCondition::AccumResources(res, amount))) |
    0x03 => do_parse!(loc: eat_location >> hall: eat_hall_level >> castle: eat_castle_level >>
                      (H3MVictoryCondition::UpgradeTown(loc, hall, castle))) |
    0x04 => map!(alt!(tag!([255u8; 3]) => { |_| None } | call!(eat_location) => { |x| Some(x) }),
        |loc| H3MVictoryCondition::BuildGrail(loc)) |
    0x05 => map!(eat_location, |loc| H3MVictoryCondition::DefeatHero(loc)) |
    0x06 => map!(eat_location, |loc| H3MVictoryCondition::CaptureTown(loc)) |
    0x07 => map!(eat_location, |loc| H3MVictoryCondition::DefeatMonster(loc)) |
    0x08 => value!(H3MVictoryCondition::FlagAllDwellings) |
    0x09 => value!(H3MVictoryCondition::FlagAllMines) |
    0x0A => map!(tuple!(eat_artifact1, eat_location), |p| H3MVictoryCondition::TransportArtifact(p.0, p.1))
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MLossCondition {
    None,
    LoseTown(H3MLocation),
    LoseHero(H3MLocation),
    TimeExpires(u16),
}

named!(eat_loss<H3MLossCondition>, switch!(eat_8,
    0xFF => value!(H3MLossCondition::None) |
    0x00 => do_parse!(l: eat_location >> (H3MLossCondition::LoseTown(l))) |
    0x01 => do_parse!(l: eat_location >> (H3MLossCondition::LoseHero(l))) |
    0x02 => do_parse!(d: eat_16 >> (H3MLossCondition::TimeExpires(d)))
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MHero {
    face: u8,
    name: String,
}

named!(eat_hero<H3MHero>, do_parse!(
    face: eat_8 >>
    name: eat_string >>
    (H3MHero {
        face, name,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MPlayerPlayability {
    human: bool,
    computer: bool,
    behavior: H3MPlayerBehavior,
}

named!(eat_player_playability<H3MPlayerPlayability>, do_parse!(
    human: eat_flag >>
    computer: eat_flag >>
    behavior: eat_player_behavior >>
    (H3MPlayerPlayability {
        human, computer, behavior,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MPlayerAllowedAlignments {
    unknown: bool, // SoD
    mask: u8,
    mask_ext: u8,
    random: bool,
}

impl Default for H3MPlayerAllowedAlignments {
    fn default() -> Self {
        Self { unknown: false, mask: 0u8, mask_ext: 0u8, random: false }
    }
}

named_args!(eat_player_allowed_alignments(version: H3MVersion, playable: bool)<H3MPlayerAllowedAlignments>,
    switch!(value!(playable),
        true => do_parse!(
            unknown: versions!(version, value!(false), value!(false), call!(eat_flag)) >>
            mask: eat_8 >>
            mask_ext: versions!(version, value!(0u8), call!(eat_8), call!(eat_8)) >>
            random: eat_flag >>
            (H3MPlayerAllowedAlignments {
                unknown, mask, mask_ext, random,
            })
        ) |
        false => value!(H3MPlayerAllowedAlignments::default(), versions!(version, take!(2), take!(3), take!(4)))
        // NOTE: if player is not playable, this contains junk, just eat it
        // TODO: this number is 3 for RoE & AB
    )
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MPlayer {
    playability: H3MPlayerPlayability,
    allowed_alignments: H3MPlayerAllowedAlignments,
    main_town: Option<H3MMainTown>,
    random_hero: bool,
    hero_type: u8,
    main_hero: Option<H3MHero>,
    num_placeholders: u8, // AB/SoD
    heroes: Vec<H3MHero>, // AB/SoD
}

named_args!(eat_player(version: H3MVersion)<H3MPlayer>, do_parse!(
    playability: eat_player_playability >>
    allowed_alignments: call!(eat_player_allowed_alignments, version, playability.human || playability.computer) >>
    main_town: eat_option!(call!(eat_main_town, version)) >>
    random_hero: eat_flag >>
    hero_type: eat_8 >>
    main_hero: switch!(value!(hero_type),
        0xFFu8 => value!(None) |
        _ => map!(eat_hero, |x| Some(x))
    ) >>
    num_placeholders: versions!(version,
        value!(0u8), call!(eat_8), call!(eat_8)
    ) >>
    heroes: versions!(version,
        value!(Vec::default()), length_count!(eat_32, eat_hero), length_count!(eat_32, eat_hero)
    ) >>
    (H3MPlayer {
        playability,
        allowed_alignments,
        main_town,
        random_hero,
        hero_type,
        main_hero,
        num_placeholders,
        heroes,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MHeroAvailability {
    id: u8,
    face: u8,
    name: String,
    players_mask: u8,
}

named!(eat_hero_availability<H3MHeroAvailability>, do_parse!(
    id: eat_8 >>
    face: eat_8 >>
    name: eat_string >>
    players_mask: eat_8 >>
    (H3MHeroAvailability {
        id, face, name, players_mask,
    })
));

#[derive(Debug)]
struct H3MAvailableHeroes {
    mask: [u8; 16],
    mask_ext: [u8; 4], // AB/SoD
    settings: Vec<H3MHeroAvailability>, // SoD
}

named_args!(eat_available_heroes(version: H3MVersion)<H3MAvailableHeroes>, do_parse!(
    mask: count_fixed!(u8, eat_8, 16) >>
    mask_ext: versions!(version, value!([0u8; 4]), count_fixed!(u8, eat_8, 4), count_fixed!(u8, eat_8, 4)) >>
    _zeroes: versions!(version, value!(()), value!((), tag!([0u8; 4])), value!((), tag!([0u8; 4]))) >>
    settings: versions!(version, value!(Vec::default()), value!(Vec::default()), length_count!(eat_8, eat_hero_availability)) >>
    (H3MAvailableHeroes {
        mask, mask_ext, settings,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MSkillLevel, eat_skill_level, eat_8>
    (0, Unspecified)
    (1, Basic)
    (2, Advanced)
    (3, Expert)
}

h3m_enum! { <H3MHeroGender, eat_hero_gender, eat_8>
    (0x00, Male)
    (0x01, Female)
    (0xFF, Default)
}

h3m_enum! { <H3MColor, eat_color, eat_8>
    (0, Red)
    (1, Blue)
    (2, Tan)
    (3, Green)
    (4, Orange)
    (5, Purple)
    (6, Teal)
    (7, Pink)
    (0xFF, Unspecified)
}

#[derive(Debug)]
struct H3MHeroEquipment {
    head: H3MArtifact,
    shoulders: H3MArtifact,
    neck: H3MArtifact,
    rhand: H3MArtifact,
    lhand: H3MArtifact,
    torso: H3MArtifact,
    rring: H3MArtifact,
    lring: H3MArtifact,
    feet: H3MArtifact,
    misc1: H3MArtifact,
    misc2: H3MArtifact,
    misc3: H3MArtifact,
    misc4: H3MArtifact,
    machine1: H3MArtifact,
    machine2: H3MArtifact,
    machine3: H3MArtifact,
    machine4: H3MArtifact,
    spellbook: H3MArtifact,
    misc5: H3MArtifact, // SoD
    backpack: Vec<H3MArtifact>,
}

named_args!(eat_hero_equipment(version: H3MVersion)<H3MHeroEquipment>, do_parse!(
    head: call!(eat_artifact, version) >>
    shoulders: call!(eat_artifact, version) >>
    neck: call!(eat_artifact, version) >>
    rhand: call!(eat_artifact, version) >>
    lhand: call!(eat_artifact, version) >>
    torso: call!(eat_artifact, version) >>
    rring: call!(eat_artifact, version) >>
    lring: call!(eat_artifact, version) >>
    feet: call!(eat_artifact, version) >>
    misc1: call!(eat_artifact, version) >>
    misc2: call!(eat_artifact, version) >>
    misc3: call!(eat_artifact, version) >>
    misc4: call!(eat_artifact, version) >>
    machine1: call!(eat_artifact, version) >>
    machine2: call!(eat_artifact, version) >>
    machine3: call!(eat_artifact, version) >>
    machine4: call!(eat_artifact, version) >>
    spellbook: call!(eat_artifact, version) >>
    misc5: versions!(version, value!(H3MArtifact(0xFFFF)), value!(H3MArtifact(0xFFFF)), call!(eat_artifact2)) >>
    backpack: length_count!(eat_16, call!(eat_artifact, version)) >>
    (H3MHeroEquipment {
        head, shoulders, neck,
        rhand, lhand, torso, rring, lring, feet,
        misc1, misc2, misc3, misc4, misc5,
        machine1, machine2, machine3, machine4,
        spellbook, backpack,
    })
));

// Whole struct is SoD only
#[derive(Debug)]
struct H3MHeroCustomization {
    exp: Option<u32>,
    skills: Option<Vec<(H3MSkill, H3MSkillLevel)>>,
    equipment: Option<H3MHeroEquipment>,
    bio: Option<String>,
    gender: H3MHeroGender,
    spells: Option<H3MSpellsMask>,
    stats: Option<(u8, u8, u8, u8)>,
}

named!(eat_hero_customization<H3MHeroCustomization>, do_parse!(
    exp: eat_option!(eat_32) >>
    skills: eat_option!(length_count!(eat_32, tuple!(eat_skill, eat_skill_level))) >>
    equipment: eat_option!(call!(eat_hero_equipment, H3MVersion::SoD)) >>
    bio: eat_option!(eat_string) >>
    gender: eat_hero_gender >>
    spells: eat_option!(eat_spells_mask) >>
    stats: eat_option!(tuple!(eat_8, eat_8, eat_8, eat_8)) >>
    (H3MHeroCustomization {
        exp, skills, equipment, bio, gender, spells, stats,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! {
    <H3MTerrainType, eat_terrain_type, eat_8, colored::Color>
    (0x00, TrDirt, colored::Color::Yellow)
    (0x01, TrSand, colored::Color::BrightYellow)
    (0x02, TrGrass, colored::Color::Green)
    (0x03, TrSnow, colored::Color::BrightWhite)
    (0x04, TrSwamp, colored::Color::Cyan)
    (0x05, TrRough, colored::Color::Magenta)
    (0x06, TrSubterranean, colored::Color::BrightBlack)
    (0x07, TrLava, colored::Color::Red)
    (0x08, TrWater, colored::Color::Blue)
    (0x09, TrRock, colored::Color::Black)
}

h3m_enum! { <H3MRiverType, eat_river_type, eat_8>
    (0x00, RvNone)
    (0x01, RvClear)
    (0x02, RvIcy)
    (0x03, RvMuddy)
    (0x04, RvLava)
}

h3m_enum! { <H3MRiverTopology, eat_river_topo, eat_8>
    (0x00, Turn1)
    (0x01, Turn2)
    (0x02, Turn3)
    (0x03, Turn4)
    (0x04, Cross)
    (0x05, TVert1)
    (0x06, TVert2)
    (0x07, THorz1)
    (0x08, THorz2)
    (0x09, Vert1)
    (0x0A, Vert2)
    (0x0B, Horz1)
    (0x0C, Horz2)
}

h3m_enum! { <H3MRoadType, eat_road_type, eat_8>
    (0x00, RdNone)
    (0x01, RdDirt)
    (0x02, RdGravel)
    (0x03, RdCobblestone)
}

h3m_enum! { <H3MRoadTopology, eat_road_topo, eat_8, &[u8]>
    (0x00, Turn1,   b".   oo o ")
    (0x01, Turn2,   b".   ** * ")
    (0x02, Turn3,   b".. . o o ")
    (0x03, Turn4,   b".. . o o ")
    (0x04, Turn5,   b".. . o o ")
    (0x05, Turn6,   b".. . o o ")
    (0x06, TVert1,  b" o  oo o ")
    (0x07, TVert2,  b" *  ** * ")
    (0x08, THorz1,  b"   ooo o ")
    (0x09, THorz2,  b"   *** * ")
    (0x0A, Vert1,   b" o  o  o ")
    (0x0B, Vert2,   b" *  *  * ")
    (0x0C, Horz1,   b"   ooo   ")
    (0x0D, Horz2,   b"   ***   ")
    (0x0E, EndVert, b". . o  o ")
    (0x0F, EndHorz, b".   oo.  ")
    (0x10, Cross,   b" o ooo o ")
}

struct H3MTile {
    terrain: H3MTerrainType,
    texture: u8,
    flip_terrain_x: bool,
    flip_terrain_y: bool,
    river_type: H3MRiverType,
    river_topo: H3MRiverTopology,
    flip_river_x: bool,
    flip_river_y: bool,
    road_type: H3MRoadType,
    road_topo: H3MRoadTopology,
    flip_road_x: bool,
    flip_road_y: bool,
}

impl std::fmt::Debug for H3MTile {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{:?}:{:?}{}{}{}{}>",
            self.terrain, self.texture,
            if self.flip_terrain_x { "-" } else { "" },
            if self.flip_terrain_y { "|" } else { "" },
            if let H3MRiverType::RvNone = self.river_type { "".to_owned() } else {
                format!(" {:?}:{:?}{}{}", self.river_type, self.river_topo,
                        if self.flip_river_x { "-" } else { "" },
                        if self.flip_river_y { "|" } else { "" },
                )
            },
            if let H3MRoadType::RdNone = self.road_type { "".to_owned() } else {
                format!(" {:?}:{:?}{}{}", self.road_type, self.road_topo,
                        if self.flip_road_x { "-" } else { "" },
                        if self.flip_road_y { "|" } else { "" },
                )
            },
        )
    }
}

named!(eat_tile<H3MTile>, do_parse!(
    terrain: eat_terrain_type >>
    texture: eat_8 >>
    river_type: eat_river_type >>
    river_topo: eat_river_topo >>
    road_type: eat_road_type >>
    road_topo: eat_road_topo >>
    mirror: eat_8 >>
    (H3MTile {
        terrain, texture,
        river_type, river_topo,
        road_type, road_topo,
        flip_terrain_y: mirror & 1 == 1,
        flip_terrain_x: mirror & 2 == 2,
        flip_river_y: mirror & 4 == 4,
        flip_river_x: mirror & 8 == 8,
        flip_road_y: mirror & 16 == 16,
        flip_road_x: mirror & 32 == 32
    })
));

struct H3MMap {
    tiles: Vec<H3MTile>,
}

impl std::fmt::Debug for H3MMap {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{} tiles>", self.tiles.len())
    }
}

#[derive(Debug)]
struct H3MObjectTemplate {
    filename: String,
    shape_mask: [u8; 6],
    visit_mask: [u8; 6],
    terrain_type_mask: u32,
    class_subclass: [u8; 8], // TODO:
    group: u8,
    is_overlay: bool,
}

named!(eat_object_template<H3MObjectTemplate>, do_parse!(
    filename: eat_string >>
    shape_mask: count_fixed!(u8, eat_8, 6) >>
    visit_mask: count_fixed!(u8, eat_8, 6) >>
    terrain_type_mask: eat_32 >>
    class_subclass: count_fixed!(u8, eat_8, 8) >>
    group: eat_8 >>
    is_overlay: eat_flag >>
    _zeroes: tag!([0u8; 16]) >>
    (H3MObjectTemplate {
        filename, shape_mask, visit_mask, terrain_type_mask,
        class_subclass, group, is_overlay,
    })
));

#[derive(Debug)]
enum H3MBuildings {
    Custom([u8; 12]),
    Fort(bool),
}

named!(eat_buildings<H3MBuildings>,
    switch!(eat_flag,
        true => map!(count_fixed!(u8, eat_8, 12), |m| H3MBuildings::Custom(m)) |
        false => map!(eat_flag, |f| H3MBuildings::Fort(f))
    )
);

#[derive(Debug)]
struct H3MTownEvent {
    event: H3MEvent,
    buildings: [u8; 6],
    creatures: [u16; 7],
    unknown: u32,
}

named_args!(eat_town_event(version: H3MVersion)<H3MTownEvent>, do_parse!(
    event: call!(eat_event, version) >>
    buildings: count_fixed!(u8, eat_8, 6) >>
    creatures: count_fixed!(u16, eat_16, 7) >>
    unknown: eat_32 >>
    (H3MTownEvent {
        event, buildings, creatures, unknown
    })
));

#[derive(Debug)]
struct H3MObjectTown {
    id: u32,
    owner: H3MColor,
    name: Option<String>,
    garrison: Option<H3MCreatures>,
    group_formation: bool,
    buildings: H3MBuildings,
    forced_spells: H3MSpellsMask,
    allowed_spells: H3MSpellsMask,
    events: Vec<H3MTownEvent>,
    alignment: u8,
}

named_args!(eat_obj_town(version: H3MVersion)<H3MObjectProperties>, do_parse!(
    id: versions!(version, value!(0xFFFFFFFF), call!(eat_32), call!(eat_32)) >>
    owner: eat_color >>
    name: eat_option!(eat_string) >>
    garrison: eat_option!(call!(eat_creatures, version)) >>
    group_formation: eat_flag >>
    buildings: eat_buildings >>
    forced_spells: versions!(version, value!(H3MSpellsMask::default()), call!(eat_spells_mask), call!(eat_spells_mask)) >>
    allowed_spells: eat_spells_mask >>
    events: length_count!(eat_32, call!(eat_town_event, version)) >>
    alignment: versions!(version, value!(0u8), value!(0u8), call!(eat_8)) >>
    _zeroes: tag!([0u8; 3]) >>
    (H3MObjectProperties::Town(H3MObjectTown{
        id, owner, name, garrison, group_formation, buildings,
        forced_spells, allowed_spells, events, alignment
    }))
));

#[derive(Debug)]
struct H3MObjectHero {
    id: u32, // AB/SoD
    owner: H3MColor,
    hero_type: u8,
    name: Option<String>,
    exp: Option<u32>, // RoE'/AB'/SoD
    face: Option<u8>,
    skills: Option<Vec<(H3MSkill, H3MSkillLevel)>>,
    garrison: Option<H3MCreatures>,
    group_formation: bool,
    equipment: Option<H3MHeroEquipment>,
    patrol_radius: u8,
    bio: Option<String>, // AB/SoD
    gender: H3MHeroGender, // AB/SoD
    spells: Option<H3MSpellsMask>, // AB'/SoD // TODO: check that one spell
    stats: Option<(u8, u8, u8, u8)>, // SoD
}

named_args!(eat_obj_hero(version: H3MVersion)<H3MObjectProperties>, do_parse!(
    id: versions!(version, value!(0xFFFFFFFF), call!(eat_32), call!(eat_32)) >>
    owner: eat_color >>
    hero_type: eat_8 >>
    name: eat_option!(eat_string) >>
    exp: versions!(version, map!(eat_32, |x| Some(x)), map!(eat_32, |x| Some(x)), eat_option!(eat_32)) >>
    face: eat_option!(eat_8) >>
    skills: eat_option!(length_count!(eat_32, tuple!(eat_skill, eat_skill_level))) >>
    garrison: eat_option!(call!(eat_creatures, version)) >>
    group_formation: eat_flag >>
    equipment: eat_option!(call!(eat_hero_equipment, version)) >>
    patrol_radius: eat_8 >>
    bio: versions!(version, value!(None), eat_option!(eat_string), eat_option!(eat_string)) >>
    gender: versions!(version, value!(H3MHeroGender::Default), call!(eat_hero_gender), call!(eat_hero_gender)) >>
    spells: versions!(version, value!(None), value!(None, eat_8), eat_option!(eat_spells_mask)) >>
    stats: versions!(version, value!(None), value!(None), eat_option!(tuple!(eat_8, eat_8, eat_8, eat_8))) >>
    _zeros: tag!([0u8; 16]) >>
    (H3MObjectProperties::Hero(H3MObjectHero {
        id, owner, hero_type, name, exp, face, skills, garrison, group_formation,
        equipment, patrol_radius, bio, gender, spells, stats,
    }))
));

#[derive(Debug)]
struct H3MObjectMonster {
    id: u32, // AB/SoD
    quantity: u16,
    mood: u8,
    reward: Option<(String, H3MResources, H3MArtifact)>,
    never_runaway: bool,
    never_grow: bool,
}

named_args!(eat_obj_monster(version: H3MVersion)<H3MObjectProperties>, do_parse!(
    id: versions!(version, value!(0xFFFFFFFF), call!(eat_32), call!(eat_32)) >>
    quantity: eat_16 >>
    mood: eat_8 >>
    reward: eat_option!(tuple!(eat_string, eat_resources, call!(eat_artifact, version))) >>
    never_runaway: eat_flag >>
    never_grow: eat_flag >>
    _zeroes: tag!([0u8; 2]) >>
    (H3MObjectProperties::Monster(H3MObjectMonster {
        id, quantity, mood, reward, never_runaway, never_grow,
    }))
));

named_args!(eat_obj_placeholder(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: eat_color >>
    id: eat_8 >>
    power_rating: switch!(value!(id == 0xFF),
                          true => map!(eat_8, |x| Some(x)) |
                          false => value!(None)
    ) >>
    (H3MObjectProperties::HeroPlaceholder {
        owner, id, power_rating
    })
));

named_args!(eat_obj_owned(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: eat_color >>
    _zeroes: tag!([0u8; 3]) >>
    (H3MObjectProperties::OwnedObject { owner })
));

#[derive(Debug)]
enum H3MDwellingFaction {
    SameAsTown(u32),
    Mask(u16),
}

named!(eat_dwelling_faction<H3MDwellingFaction>,
    switch!(peek!(eat_32),
        0 => do_parse!(_zero: eat_32 >> m: eat_16 >> (H3MDwellingFaction::Mask(m))) |
        _ => map!(eat_32, |t| H3MDwellingFaction::SameAsTown(t))
    )
);

named_args!(eat_obj_dwelling(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: eat_color >>
    _zeroes: tag!([0u8; 3]) >>
    faction: eat_dwelling_faction >>
    level_range: tuple!(eat_8, eat_8) >>
    (H3MObjectProperties::RandomDwelling { owner, faction, level_range })
));

named_args!(eat_obj_dwelling_level(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: eat_color >>
    _zeroes: tag!([0u8; 3]) >>
    faction: eat_dwelling_faction >>
    (H3MObjectProperties::RandomDwellingLevel { owner, faction  })
));

named_args!(eat_obj_dwelling_faction(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: eat_color >>
    _zeroes: tag!([0u8; 3]) >>
    level_range: tuple!(eat_8, eat_8) >>
    (H3MObjectProperties::RandomDwellingFaction { owner, level_range })
));

named_args!(eat_obj_resource(version: H3MVersion)<H3MObjectProperties>, do_parse!(
    guard: eat_option!(call!(eat_msg_guards, version)) >>
    amount: eat_32 >>
    _zeroes: tag!([0u8; 4]) >>
    (H3MObjectProperties::Resource { guard, amount })
));

#[derive(Debug)]
struct H3MMessageAndGuards {
    message: String,
    guards: Option<H3MCreatures>,
}

named_args!(eat_msg_guards(version: H3MVersion)<H3MMessageAndGuards>, do_parse!(
    message: eat_string >>
    guards: eat_option!(call!(eat_creatures, version)) >>
    _zeroes: tag!([0u8; 4]) >>
    (H3MMessageAndGuards { message, guards })
));

#[derive(Debug)]
struct H3MCreatures([(H3MCreature, u16); 7]);

named_args!(eat_creatures(version: H3MVersion)<H3MCreatures>,
    map!(count_fixed!((H3MCreature, u16), tuple!(call!(eat_creature, version), eat_16), 7), |cs| H3MCreatures(cs))
);

named_args!(eat_obj_artifact(version: H3MVersion)<H3MObjectProperties>,
    map!(eat_option!(call!(eat_msg_guards, version)), |guard| H3MObjectProperties::Artifact { guard })
);

named_args!(eat_obj_scroll(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        guard: eat_option!(call!(eat_msg_guards, version)) >>
        spell: eat_spell >>
        _zeros: tag!([0u8; 3]) >>
        (H3MObjectProperties::Scroll { guard, spell })
    )
);

named_args!(eat_obj_witch(version: H3MVersion)<H3MObjectProperties>, map!(
    versions!(version, value!(0x0FFFEFBF), call!(eat_32), call!(eat_32)),
    |skills| H3MObjectProperties::Witch { skills }
));

named_args!(eat_obj_shrine(_v: H3MVersion)<H3MObjectProperties>,
    do_parse!(spell: eat_8 >> _zeroes: tag!([0u8; 3]) >> (H3MObjectProperties::Shrine { spell }))
);

named_args!(eat_obj_grail(_v: H3MVersion)<H3MObjectProperties>,
    map!(eat_32, |radius| H3MObjectProperties::Grail { radius })
);

named_args!(eat_obj_message(_v: H3MVersion)<H3MObjectProperties>,
    do_parse!(text: eat_string >> _zeroes: tag!([0u8; 4]) >> (H3MObjectProperties::Message { text }))
);

named_args!(eat_obj_scholar(_v: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        bonus_type: eat_8 >>
        bonus_id: eat_8 >>
        _zeroes: tag!([0u8; 6]) >>
        (H3MObjectProperties::Scholar { bonus_type, bonus_id })
    )
);

named_args!(eat_obj_abandoned(_v: H3MVersion)<H3MObjectProperties>,
    do_parse!(resources: eat_8 >> _zeroes: tag!([0u8; 3]) >> (H3MObjectProperties::AbandonedMine { resources }))
);

named_args!(eat_obj_garrison(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        owner: eat_color >>
        _zeroes1: tag!([0u8; 3]) >>
        creatures: call!(eat_creatures, version) >>
        removable: versions!(version, value!(0u8), call!(eat_8), call!(eat_8)) >> // TODO: bool?
        _zeroes2: tag!([0u8; 8]) >>
        (H3MObjectProperties::Garrison {
            owner, creatures, removable
        })
    )
);

h3m_enum! { <H3MStat, eat_stat, eat_8>
    (0, Attack)
    (1, Defense)
    (2, SpellPower)
    (3, Knowledge)
}

h3m_enum! { <H3MSkill, eat_skill, eat_8>
    (0, Pathfinding)
    (1, Archery)
    (2, Logistics)
    (3, Scouting)
    (4, Diplomacy)
    (5, Navigation)
    (6, Leadership)
    (7, Wisdom)
    (8, Mysticism)
    (9, Luck)
    (10, Ballistics)
    (11, EagleEye)
    (12, Necromancy)
    (13, Estates)
    (14, FireMagic)
    (15, AirMagic)
    (16, WaterMagic)
    (17, EarthMagic)
    (18, Scholar)
    (19, Tactics)
    (20, Artillery)
    (21, Learning)
    (22, Offence)
    (23, Armorer)
    (24, Intelligence)
    (25, Sorcery)
    (26, Resistance)
    (27, FirstAid)
}

h3m_enum! { <H3MModifier, eat_modifier, eat_8>
    (0, None)
    (1, Plus1)
    (2, Plus2)
    (3, Plus3)
    (255, Minus1)
    (254, Minus2)
    (253, Minus3)
}

#[derive(Debug)]
struct H3MSpell(u8);

named!(eat_spell<H3MSpell>, map!(eat_8, |s| H3MSpell(s)));

#[derive(Debug)]
enum H3MReward {
    None,
    Exp(u32),
    SpellPoints(u32),
    Morale(H3MModifier),
    Luck(H3MModifier),
    Resource(H3MResource, u32),
    Stat(H3MStat, u8),
    Skill(H3MSkill, H3MSkillLevel),
    Artifact(H3MArtifact),
    Spell(H3MSpell),
    Creatures(H3MCreature, u16),
}

named_args!(eat_reward(version: H3MVersion)<H3MReward>,
    switch!(eat_8,
        0 => value!(H3MReward::None) |
        1 => map!(eat_32, |x| H3MReward::Exp(x)) |
        2 => map!(eat_32, |x| H3MReward::SpellPoints(x)) |
        3 => map!(eat_modifier, |m| H3MReward::Morale(m)) |
        4 => map!(eat_modifier, |m| H3MReward::Luck(m)) |
        5 => do_parse!(r: eat_resource >> n: eat_32 >> (H3MReward::Resource(r, n))) |
        6 => do_parse!(s: eat_stat >> n: eat_8 >> (H3MReward::Stat(s, n))) |
        7 => do_parse!(s: eat_skill >> l: eat_skill_level >> (H3MReward::Skill(s, l))) |
        8 => map!(call!(eat_artifact, version), |a| H3MReward::Artifact(a)) |
        9 => map!(eat_spell, |s| H3MReward::Spell(s)) |
        10 => do_parse!(c: call!(eat_creature, version) >>
                        n: eat_16 >> (H3MReward::Creatures(c, n)))
    )
);

// Whole struct is AB/SoD
#[derive(Debug)]
enum H3MQuestObjective {
    None,
    Level(u32),
    Stats((u8, u8, u8, u8)),
    DefeatHero(u32), // TODO
    DefeatMonster(u32), // TODO
    Artifacts(Vec<H3MArtifact>),
    Creatures(Vec<(H3MCreature, u16)>),
    Resources(H3MResources),
    Hero(u8),
    Color(H3MColor), // TODO: what?
}

named!(eat_quest_objective<H3MQuestObjective>,
    switch!(eat_8,
        0 => value!(H3MQuestObjective::None) |
        1 => map!(eat_32, |x| H3MQuestObjective::Level(x)) |
        2 => map!(tuple!(eat_8, eat_8, eat_8, eat_8),
                  |x| H3MQuestObjective::Stats(x)) |
        3 => map!(eat_32, |x| H3MQuestObjective::DefeatHero(x)) |
        4 => map!(eat_32, |x| H3MQuestObjective::DefeatMonster(x)) |
        5 => map!(length_count!(eat_8, eat_artifact2),
                  |x| H3MQuestObjective::Artifacts(x)) |
        6 => map!(length_count!(eat_8, tuple!(eat_creature2, eat_16)),
                  |x| H3MQuestObjective::Creatures(x)) |
        7 => map!(eat_resources, |x| H3MQuestObjective::Resources(x)) |
        8 => map!(eat_8, |x| H3MQuestObjective::Hero(x)) |
        9 => map!(eat_color, |x| H3MQuestObjective::Color(x))
    )
);

#[derive(Debug)]
struct H3MQuest {
    objective: H3MQuestObjective,
    deadline: u32,
    text_start: String,
    text_repeat: String,
    text_complete: String,
}

// RoE
named!(eat_quest1<H3MQuest>,
    map!(eat_artifact1,
        |a| H3MQuest {
            objective: H3MQuestObjective::Artifacts(vec![a]),
            deadline: 0xFFFFFFFF,
            text_start: String::default(),
            text_repeat: String::default(),
            text_complete: String::default(),
        }
    )
);

// AB/SoD
named!(eat_quest2<H3MQuest>,
    do_parse!(
        objective: eat_quest_objective >>
        deadline: eat_32 >>
        text_start: eat_string >>
        text_repeat: eat_string >>
        text_complete: eat_string >>
        (H3MQuest {
            objective, deadline, text_start, text_repeat, text_complete
        })
    )
);

named_args!(eat_obj_seer(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        quest: versions!(version, call!(eat_quest1), call!(eat_quest2), call!(eat_quest2)) >>
        reward: call!(eat_reward, version) >>
        _zeroes: tag!([0u8; 2]) >>
        (H3MObjectProperties::Seer { quest, reward })
    )
);

named_args!(eat_obj_quest_guard(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        quest: versions!(version, call!(eat_quest1), call!(eat_quest2), call!(eat_quest2)) >>
        (H3MObjectProperties::QuestGuard { quest })
    )
);

#[derive(Debug)]
struct H3MMsgGuardReward {
    guard: Option<H3MMessageAndGuards>,
    exp: u32,
    spell_points: u32,
    morale: H3MModifier,
    luck: H3MModifier,
    resources: H3MResources,
    stats: (u8, u8, u8, u8),
    skills: Vec<(H3MSkill, H3MSkillLevel)>,
    artifacts: Vec<H3MArtifact>,
    spells: Vec<H3MSpell>,
    creatures: Vec<(H3MCreature, u16)>,
}

named_args!(eat_msg_guard_reward(version: H3MVersion)<H3MMsgGuardReward>,
    do_parse!(
        guard: eat_option!(call!(eat_msg_guards, version)) >>
        exp: eat_32 >>
        spell_points: eat_32 >>
        morale: eat_modifier >>
        luck: eat_modifier >>
        resources: eat_resources >>
        stats: tuple!(eat_8, eat_8, eat_8, eat_8) >>
        skills: length_count!(eat_8, tuple!(eat_skill, eat_skill_level)) >>
        artifacts: length_count!(eat_8, call!(eat_artifact, version)) >>
        spells: length_count!(eat_8, eat_spell) >>
        creatures: length_count!(eat_8, tuple!(call!(eat_creature, version), eat_16)) >>
        _zeroes: tag!([0u8; 8]) >>
        (H3MMsgGuardReward {
            guard, exp, spell_points, morale, luck,
            resources, stats, skills, artifacts, spells, creatures,
        })
    )
);

named_args!(eat_obj_pandora(version: H3MVersion)<H3MObjectProperties>, map!(
    call!(eat_msg_guard_reward, version),
    |contents| H3MObjectProperties::Pandora { contents }
));

named_args!(eat_obj_event(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        contents: call!(eat_msg_guard_reward, version) >>
        players_mask: eat_8 >>
        ai_allowed: eat_flag >>
        one_time: eat_flag >>
        _zeroes: tag!([0u8; 4]) >>
        (H3MObjectProperties::Event {
            contents, players_mask, ai_allowed, one_time
        })
    )
);

#[derive(Debug)]
enum H3MObjectProperties {
    Hero(H3MObjectHero),
    Monster(H3MObjectMonster),
    Town(H3MObjectTown),
    HeroPlaceholder { owner: H3MColor, id: u8, power_rating: Option<u8> },
    OwnedObject { owner: H3MColor },
    RandomDwelling { owner: H3MColor, faction: H3MDwellingFaction, level_range: (u8, u8) },
    RandomDwellingLevel { owner: H3MColor, faction: H3MDwellingFaction },
    RandomDwellingFaction { owner: H3MColor, level_range: (u8, u8) },
    Resource { guard: Option<H3MMessageAndGuards>, amount: u32 },
    Artifact { guard: Option<H3MMessageAndGuards> },
    Scroll { guard: Option<H3MMessageAndGuards>, spell: H3MSpell },
    Witch { skills: u32 },
    Shrine { spell: u8 },
    Grail { radius: u32 },
    Message { text: String },
    Scholar { bonus_type: u8, bonus_id: u8 },
    AbandonedMine { resources: u8 },
    Garrison { owner: H3MColor, creatures: H3MCreatures, removable: u8 },
    Seer { quest: H3MQuest, reward: H3MReward },
    QuestGuard { quest: H3MQuest },
    Pandora { contents: H3MMsgGuardReward },
    Event { contents: H3MMsgGuardReward, players_mask: u8, ai_allowed: bool, one_time: bool },
    NoProperties,
}

named_args!(eat_obj_noprops(_v: H3MVersion)<H3MObjectProperties>, value!(H3MObjectProperties::NoProperties));

h3m_enum! { <H3MObjectClass, eat_obj_class, eat_32, fn (&[u8], H3MVersion) -> nom::IResult<&[u8], H3MObjectProperties>>
    // Objects without additional properties
    (2, AltarOfSacrifice, eat_obj_noprops)
    (4, Arena, eat_obj_noprops)
    (7, BlackMarket, eat_obj_noprops)
    (8, Boat, eat_obj_noprops)
    (9, BorderGuard, eat_obj_noprops)
    (10, KeymastersTent, eat_obj_noprops)
    (11, Buoy, eat_obj_noprops)
    (12, Campfire, eat_obj_noprops)
    (13, Cartographer, eat_obj_noprops)
    (14, SwanPond, eat_obj_noprops)
    (15, CoverOfDarkness, eat_obj_noprops)
    (16, CreatureBank, eat_obj_noprops)
    (21, CursedGround1, eat_obj_noprops)
    (22, Corpse, eat_obj_noprops)
    (23, MarlettoTower, eat_obj_noprops)
    (24, DerelictShip, eat_obj_noprops)
    (25, DragonUtopia, eat_obj_noprops)
    (27, EyeOfMagi, eat_obj_noprops)
    (28, FaerieRing, eat_obj_noprops)
    (29, Flotsam, eat_obj_noprops)
    (30, FountainOfFortune, eat_obj_noprops)
    (31, FountainOfYouth, eat_obj_noprops)
    (32, GardenOfRevelation, eat_obj_noprops)
    (35, HillFort, eat_obj_noprops)
    (37, HutOfMagi, eat_obj_noprops)
    (38, IdolOfFortune, eat_obj_noprops)
    (39, LeanTo, eat_obj_noprops)
    (41, LibraryOfEnlightenment, eat_obj_noprops)
    (43, MonolithEntrance, eat_obj_noprops)
    (44, MonolithExit, eat_obj_noprops)
    (45, MonolithTwoWay, eat_obj_noprops)
    (46, MagicPlains1, eat_obj_noprops)
    (47, SchoolOfMagic, eat_obj_noprops)
    (48, MagicSpring, eat_obj_noprops)
    (49, MagicWell, eat_obj_noprops)
    (50, MarketOfTime, eat_obj_noprops)
    (51, MercenaryCamp, eat_obj_noprops)
    (52, Mermaid, eat_obj_noprops)
    (55, MysticalGarden, eat_obj_noprops)
    (56, Oasis, eat_obj_noprops)
    (57, Obelisk, eat_obj_noprops)
    (58, RedwoodObservatory, eat_obj_noprops)
    (60, PillarOfFire, eat_obj_noprops)
    (61, StarAxis, eat_obj_noprops)
    (63, Pyramid, eat_obj_noprops)
    (64, RallyFlag, eat_obj_noprops)
    (78, RefugeeCamp, eat_obj_noprops)
    (80, Sanctuary, eat_obj_noprops)
    (82, SeaChest, eat_obj_noprops)
    (84, Crypt, eat_obj_noprops)
    (85, Shipwreck, eat_obj_noprops)
    (86, ShipwreckSurvivor, eat_obj_noprops)
    (92, Sirens, eat_obj_noprops)
    (94, Stables, eat_obj_noprops)
    (95, Tavern, eat_obj_noprops)
    (96, Temple, eat_obj_noprops)
    (97, DenOfThieves, eat_obj_noprops)
    (99, TradingPost, eat_obj_noprops)
    (100, LearningStone, eat_obj_noprops)
    (101, TreasureChest, eat_obj_noprops)
    (102, TreeOfKnowledge, eat_obj_noprops)
    (103, SubterraneanGate, eat_obj_noprops)
    (104, University, eat_obj_noprops)
    (105, Wagon, eat_obj_noprops)
    (106, WarMachineFactory, eat_obj_noprops)
    (107, SchoolOfWar, eat_obj_noprops)
    (108, WarriorsTomb, eat_obj_noprops)
    (109, WaterWheel, eat_obj_noprops)
    (110, WateringHole, eat_obj_noprops)
    (111, Whirlpool, eat_obj_noprops)
    (112, Windmill, eat_obj_noprops)
    (116, Cactus, eat_obj_noprops)
    (117, Canyon, eat_obj_noprops)
    (118, Crater, eat_obj_noprops)
    (119, DeadVegetation, eat_obj_noprops)
    (120, Flowers, eat_obj_noprops)
    (121, FrozenLake, eat_obj_noprops)
    (124, Hole, eat_obj_noprops)
    (125, Kelp, eat_obj_noprops)
    (126, Lake, eat_obj_noprops)
    (127, LavaFlow, eat_obj_noprops)
    (128, LavaLake, eat_obj_noprops)
    (129, Mushrooms, eat_obj_noprops)
    (130, Log, eat_obj_noprops)
    (131, Mandrake, eat_obj_noprops)
    (132, Moss, eat_obj_noprops)
    (133, Mound, eat_obj_noprops)
    (134, Mountain, eat_obj_noprops)
    (135, OakTrees, eat_obj_noprops)
    (136, Outcropping, eat_obj_noprops)
    (137, PineTrees, eat_obj_noprops)
    (143, RiverDelta, eat_obj_noprops)
    (147, Rock, eat_obj_noprops)
    (148, SandDune, eat_obj_noprops)
    (149, SandPit, eat_obj_noprops)
    (150, Shrub, eat_obj_noprops)
    (151, Skull, eat_obj_noprops)
    (153, Stump, eat_obj_noprops)
    (155, Trees, eat_obj_noprops)
    (158, Volcano, eat_obj_noprops)
    (161, Reef, eat_obj_noprops)
    (177, Lake2, eat_obj_noprops)
    (199, Trees2, eat_obj_noprops)
    (206, DesertHills, eat_obj_noprops)
    (207, DirtHills, eat_obj_noprops)
    (208, GrassHills, eat_obj_noprops)
    (209, RoughHills, eat_obj_noprops)
    (210, SubterraneanRocks, eat_obj_noprops)
    (211, SwampFoliage, eat_obj_noprops)
    (212, BorderGate, eat_obj_noprops)
    (213, FreelancersGuild, eat_obj_noprops)
    (221, TradingPostSnow, eat_obj_noprops)
    (222, Cloverfield, eat_obj_noprops)
    (223, CursedGround2, eat_obj_noprops)
    (224, EvilFog, eat_obj_noprops)
    (225, FavorableWinds, eat_obj_noprops)
    (226, FieryFields, eat_obj_noprops)
    (227, HolyGrounds, eat_obj_noprops)
    (228, LucidPools, eat_obj_noprops)
    (229, MagicClouds, eat_obj_noprops)
    (230, MagicPlains2, eat_obj_noprops)
    (231, RockLands, eat_obj_noprops)

    // Objects with additional properties:
    (5, Artifact, eat_obj_artifact)
    (6, PandorasBox, eat_obj_pandora)
    (17, CreatureGenerator1, eat_obj_owned)
    (18, CreatureGenerator2, eat_obj_owned)
    (19, CreatureGenerator3, eat_obj_owned)
    (20, CreatureGenerator4, eat_obj_owned)
    (26, Event, eat_obj_event)
    (33, Garrison, eat_obj_garrison)
    (34, Hero, eat_obj_hero)
    (36, Grail, eat_obj_grail)
    (42, Lighthouse, eat_obj_owned)
    (53, Mine, eat_obj_owned)
    (54, Monster, eat_obj_monster)
    (59, OceanBottle, eat_obj_message)
    (62, Prison, eat_obj_hero)
    (65, RandomArtifact, eat_obj_artifact)
    (66, RandomTreasureArtifact, eat_obj_artifact)
    (67, RandomMinorArtifact, eat_obj_artifact)
    (68, RandomMajorArtifact, eat_obj_artifact)
    (69, RandomRelicArtifact, eat_obj_artifact)
    (70, RandomHero, eat_obj_hero)
    (71, RandomMonster, eat_obj_monster)
    (72, RandomMonster1, eat_obj_monster)
    (73, RandomMonster2, eat_obj_monster)
    (74, RandomMonster3, eat_obj_monster)
    (75, RandomMonster4, eat_obj_monster)
    (76, RandomResource, eat_obj_resource)
    (77, RandomTown, eat_obj_town)
    (79, Resource, eat_obj_resource)
    (81, Scholar, eat_obj_scholar)
    (83, SeerHut, eat_obj_seer)
    (87, Shipyard, eat_obj_owned)
    (88, ShrineOfMagicIncantation, eat_obj_shrine)
    (89, ShrineOfMagicGesture, eat_obj_shrine)
    (90, ShrineOfMagicThought, eat_obj_shrine)
    (91, Sign, eat_obj_message)
    (93, SpellScroll, eat_obj_scroll)
    (98, Town, eat_obj_town)
    (113, WitchHut, eat_obj_witch)
    (162, RandomMonster5, eat_obj_monster)
    (163, RandomMonster6, eat_obj_monster)
    (164, RandomMonster7, eat_obj_monster)
    (214, HeroPlaceholder, eat_obj_placeholder)
    (215, QuestGuard, eat_obj_quest_guard)
    (216, RandomDwelling, eat_obj_dwelling)
    (217, RandomDwellingLevel, eat_obj_dwelling_level)
    (218, RandomDwellingFaction, eat_obj_dwelling_faction)
    (219, Garrison2, eat_obj_garrison)
    (220, AbandonedMine, eat_obj_abandoned)
}

impl Clone for H3MObjectClass {
    fn clone(&self) -> Self { *self }
}

impl Copy for H3MObjectClass {}

#[derive(Debug)]
struct H3MObject {
    loc: H3MLocation,
    template_idx: u32,
    properties: H3MObjectProperties,
}

fn make_class(inp: &[u8; 8]) -> u32 {
    let mut id = inp[0] as u32;
    id += (inp[1] as u32) << 8;
    id += (inp[2] as u32) << 16;
    id += (inp[3] as u32) << 24;

    id
}

fn make_subclass(inp: &[u8; 8]) -> u32 {
    let mut subid = inp[4] as u32;
    subid += (inp[5] as u32) << 8;
    subid += (inp[6] as u32) << 16;
    subid += (inp[7] as u32) << 24;

    subid
}

fn eat_obj_class_or_panic(inp: &[u8; 8]) -> H3MObjectClass {
    match eat_obj_class(inp) {
        Ok((_, class)) =>
            if let (H3MObjectClass::Mine, 7) = (class, make_subclass(inp)) {
                H3MObjectClass::AbandonedMine
            } else { class },
        _ => panic!("error parsing class: {}", make_class(inp)),
    }
}

named_args!(eat_object<'a>(version: H3MVersion, templates: &'a[H3MObjectTemplate])<H3MObject>, do_parse!(
    loc: eat_location >>
    template_idx: eat_32 >>
    _zeroes: tag!([0u8; 5]) >>
    class: value!(eat_obj_class_or_panic(&templates[template_idx as usize].class_subclass)) >>
//    _debug: value!({
//        let bytes = &templates[template_idx as usize].class_subclass;
//        println!("reading props for {}/{} -> {:?}", make_class(bytes), make_subclass(bytes), class);
//    }) >>
    properties: call!(class.to_debug(), version) >>
    (H3MObject {
        loc, template_idx, properties
    })
));

#[derive(Debug)]
struct H3MEvent {
    name: String,
    text: String,
    resources: H3MResources,
    unknown1: u8,
    unknown2: bool, // SoD
    unknown3: bool,
    first_occurence: u16,
    repeat_period: u16,
}

named_args!(eat_event(version: H3MVersion)<H3MEvent>, do_parse!(
    name: eat_string >>
    text: eat_string >>
    resources: eat_resources >>
    unknown1: eat_8 >>
    unknown2: versions!(version, value!(true), value!(true), call!(eat_flag)) >>
    unknown3: eat_flag >>
    first_occurence: eat_16 >>
    repeat_period: eat_16 >>
    _zeroes: tag!([0u8; 16]) >>
    (H3MEvent {
        name, text, resources, unknown1, unknown2, unknown3, first_occurence, repeat_period
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MFile {
    header: H3MHeader,
    players: Vec<H3MPlayer>, // TODO: this always has 8 items
    victory: Option<H3MSpecialVictoryCondition>,
    loss: H3MLossCondition,
    teams: Option<[u8; 8]>,
    available_heroes: H3MAvailableHeroes,
    banned_artifacts: [u8; 17], // AB/SoD
    banned_artifacts_ext: u8, // SoD
    banned_spells: H3MSpellsMask, // SoD
    banned_skills: u32, // SoD
    rumors: Vec<(String, String)>,
    heroes: Vec<Option<H3MHeroCustomization>>, // SoD // TODO: this always has 156 items
    land: H3MMap,
    underground: Option<H3MMap>,
    object_templates: Vec<H3MObjectTemplate>,
    objects: Vec<H3MObject>,
    events: Vec<H3MEvent>,
}

named!(eat_h3m<H3MFile>, do_parse!(
    header: eat_header >>
    players: count!(call!(eat_player, header.version), 8) >>
    victory: call!(eat_special_victory, header.version) >>
    loss: eat_loss >>
    teams: switch!(eat_8,
        0u8 => value!(None) |
        _ => map!(count_fixed!(u8, eat_8, 8), |x| Some(x))
    ) >>
    available_heroes: call!(eat_available_heroes, header.version) >>
    _zeroes: tag!([0u8; 31]) >>
    banned_artifacts: versions!(header.version, value!([0u8; 17]), count_fixed!(u8, eat_8, 17), count_fixed!(u8, eat_8, 17)) >>
    banned_artifacts_ext: versions!(header.version, value!(31u8), value!(31u8), call!(eat_8)) >>
    banned_spells: versions!(header.version, value!(H3MSpellsMask::default()), value!(H3MSpellsMask::default()), call!(eat_spells_mask)) >>
    banned_skills: versions!(header.version, value!(0u32), value!(0u32), call!(eat_32)) >>
    rumors: length_count!(eat_32, tuple!(eat_string, eat_string)) >>
    heroes: count!(versions!(header.version, value!(None), value!(None), eat_option!(eat_hero_customization)), 156) >>
    land: count!(eat_tile, header.get_width() * header.get_height()) >>
    underground: switch!(value!(header.has_underground),
        false => value!(None) |
        true => map!(count!(eat_tile, header.get_width() * header.get_height()), |tiles| Some(H3MMap { tiles }))
    ) >>
    object_templates: length_count!(eat_32, eat_object_template) >>
    objects: length_count!(eat_32, call!(eat_object, header.version, &object_templates)) >>
    events: length_count!(eat_32, call!(eat_event, header.version)) >>
    _trailing_zeroes: count!(tag!([0u8]), 124) >>
    (H3MFile {
        header, players, victory, loss, teams, available_heroes,
        banned_artifacts, banned_artifacts_ext, banned_spells, banned_skills, rumors, heroes,
        land: H3MMap { tiles: land }, underground,
        object_templates, objects, events
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

fn print_map(doc: &H3MFile) {
    let w = doc.header.get_width();
    let h = doc.header.get_height();

    let mut line = String::new();
    for r in 0..h {
        for sub in 0..3 {
            for c in 0..w {
                line.clear();
                let tile = &doc.land.tiles[r * w + c];
                if let H3MRoadType::RdNone = tile.road_type {
                    line.push_str("...");
                } else {
                    let pat = tile.road_topo.to_debug();
                    let base = 3 * if tile.flip_road_x { [2, 1, 0] } else { [0, 1, 2] } [sub];
                    if tile.flip_road_y {
                        line.push(pat[base + 2] as char);
                        line.push(pat[base + 1] as char);
                        line.push(pat[base + 0] as char);
                    } else {
                        line.push(pat[base + 0] as char);
                        line.push(pat[base + 1] as char);
                        line.push(pat[base + 2] as char);
                    }
                }
                print!("{}", line.color(tile.terrain.to_debug()))
            }
            println!();
        }
    }
}

fn main() {
    let read_file = |f| {
        let br = BufReader::new(f);
        let mut buf: Vec<u8> = Vec::new();
        GzDecoder::new(br).read_to_end(&mut buf).map(move |_| buf)
    };

    let argument = std::env::args().nth(1).ok_or("no arguments specified".to_owned());

    let res = argument.and_then(|p| File::open(p).and_then(read_file).map_err(|e| e.to_string()));

    match res {
        Ok(bin) => {
            // println!("unzipped size: {}", bin.len());

            if false {
                use std::fmt::Write;
                let mut dump = String::new();
                let mut count: usize = 0;
                for byte in &bin {
                    write!(dump, "{:02X} ", byte).unwrap();
                    count += 1;
                    if count % 32 == 0 {
                        dump.push_str("\n");
                    }
                }
                println!("\n{}", dump);
            }

            match eat_h3m(&bin) {
                Ok((rem, doc)) => {
                    // println!("parsed document: {:#?}", doc);

                    if false {
                        print_map(&doc);
                    }

                    if rem.len() > 0 {
                        panic!("remaining: {:?}", rem.len());
                    }
                }
                Err(e) => panic!("error: {:#?}", e),
            }
        },
        Err(err) => panic!("error: {}", err),
    }
}
