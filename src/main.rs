extern crate flate2;
extern crate colored;
use colored::Colorize;

#[macro_use]
extern crate nom;

use std::fs::File;
use std::io::{BufReader, Read};
use flate2::bufread::GzDecoder;

use nom::{le_u8, le_u16, le_u32};

named!(eat_8<u8>, call!(le_u8));
named!(eat_16<u16>, call!(le_u16));
named!(eat_32<u32>, call!(le_u32));

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
    level_cap: u8, // TODO: AB/SoD only?
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
    level_cap: eat_8 >>
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

#[derive(Debug)]
struct H3MLocation(u8, u8, bool);

named!(eat_location<H3MLocation>, do_parse!(
    x: eat_8 >> y: eat_8 >> u: eat_flag >>
    (H3MLocation(x, y, u))
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MMainTown {
    generate_hero: bool,
    kind: H3MTownKind,
    location: H3MLocation,
}

named!(eat_main_town<H3MMainTown>, do_parse!(
    generate_hero: eat_flag >>
    kind: eat_town_kind >>
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

#[derive(Debug)]
struct H3MArtifact(u8);

named!(artifact<H3MArtifact>, map!(eat_8, |i| H3MArtifact(i)));

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

#[derive(Debug)]
struct H3MCreature(u16);

named!(eat_creature<H3MCreature>, map!(eat_16, |i| H3MCreature(i)));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MSpecialVictoryCondition {
    condition: H3MVictoryCondition,
    or_default: bool,
    cpu_allowed: bool,
}

named!(eat_special_victory<Option<H3MSpecialVictoryCondition>>,
       switch!(peek!(eat_8),
               0xFF => value!(None, eat_8) |
               _ => do_parse!(
                       code: eat_8 >>
                       or_default: eat_flag >>
                       cpu_allowed: eat_flag >>
                       condition: call!(eat_victory, code) >>
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
    BuildGrail(H3MLocation),
    DefeatHero(H3MLocation),
    CaptureTown(H3MLocation),
    DefeatMoster(H3MLocation),
    FlagAllDwellings,
    FlagAllMines,
    TransportArtifact(H3MArtifact, H3MLocation),
}

named_args!(eat_victory(code: u8)<H3MVictoryCondition>, switch!(value!(code),
    0x00 => do_parse!(art: artifact >> (H3MVictoryCondition::AcquireArtifact(art))) |
    0x01 => do_parse!(cr: eat_creature >> amount: eat_32 >> (H3MVictoryCondition::AccumCreatures(cr, amount))) |
    0x02 => do_parse!(res: eat_resource >> amount: eat_32 >> (H3MVictoryCondition::AccumResources(res, amount))) |
    0x03 => do_parse!(loc: eat_location >> hall: eat_hall_level >> castle: eat_castle_level >>
                      (H3MVictoryCondition::UpgradeTown(loc, hall, castle))) |
    0x04 => do_parse!(loc: eat_location >> (H3MVictoryCondition::BuildGrail(loc))) |
    0x05 => do_parse!(loc: eat_location >> (H3MVictoryCondition::DefeatHero(loc))) |
    0x06 => do_parse!(loc: eat_location >> (H3MVictoryCondition::CaptureTown(loc))) |
    0x07 => do_parse!(loc: eat_location >> (H3MVictoryCondition::DefeatMoster(loc))) |
    0x08 => value!(H3MVictoryCondition::FlagAllDwellings) |
    0x09 => value!(H3MVictoryCondition::FlagAllMines) |
    0x0A => do_parse!(art: artifact >> loc: eat_location >> (H3MVictoryCondition::TransportArtifact(art, loc)))
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
    customized: bool,
    mask: u16,
    random: bool,
}

named!(eat_player_allowed_alignments<H3MPlayerAllowedAlignments>, do_parse!(
    customized: eat_flag >>
    mask: eat_16 >>
    random: eat_flag >>
    (H3MPlayerAllowedAlignments {
        customized, mask, random,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MPlayer {
    playability: H3MPlayerPlayability,
    allowed_alignments: H3MPlayerAllowedAlignments,
    main_town: Option<H3MMainTown>,
    random_hero: bool,
    hero_type: u8,
    main_hero: Option<H3MHero>,
    num_placeholders: u8,
    heroes: Vec<H3MHero>,
}

named!(eat_player<H3MPlayer>, do_parse!(
    playability: eat_player_playability >>
    allowed_alignments: switch!(value!(playability.human || playability.computer),
        true => call!(eat_player_allowed_alignments) |
        false => value!(H3MPlayerAllowedAlignments {
            customized: false, mask: 0u16, random: false,
        }, take!(4)) // NOTE: if player is not playable, this contains junk, just eat it
    ) >>
    main_town: eat_option!(eat_main_town) >>
    random_hero: eat_flag >>
    hero_type: eat_8 >>
    main_hero: switch!(value!(hero_type),
        0xFFu8 => value!(None) |
        _ => map!(eat_hero, |x| Some(x))
    ) >>
    num_placeholders: eat_8 >>
    heroes: length_count!(eat_32, eat_hero) >>
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
    mask: [u8; 20],
    settings: Vec<H3MHeroAvailability>,
}

named!(eat_available_heroes<H3MAvailableHeroes>, do_parse!(
    mask: count_fixed!(u8, eat_8, 20) >>
    zeroes: tag!(&[0u8; 4]) >>
    settings: length_count!(eat_8, eat_hero_availability) >>
    (H3MAvailableHeroes {
        mask, settings,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

h3m_enum! { <H3MSkillLevel, eat_skill_level, eat_8>
    (0, Basic)
    (1, Advanced)
    (2, Expert)
}

h3m_enum! { <H3MHeroGender, eat_hero_gender, eat_8>
    (0x00, Male)
    (0x01, Female)
    (0xFF, Default)
}

#[derive(Debug)]
struct H3MHeroEquipment {
    head: u16,
    shoulders: u16,
    neck: u16,
    rhand: u16,
    lhand: u16,
    torso: u16,
    rring: u16,
    lring: u16,
    feet: u16,
    misc1: u16,
    misc2: u16,
    misc3: u16,
    misc4: u16,
    machine1: u16,
    machine2: u16,
    machine3: u16,
    machine4: u16,
    spellbook: u16,
    misc5: u16,
    backpack: Vec<u16>,
}

named!(eat_hero_equipment<H3MHeroEquipment>, do_parse!(
    head: eat_16 >>
    shoulders: eat_16 >>
    neck: eat_16 >>
    rhand: eat_16 >>
    lhand: eat_16 >>
    torso: eat_16 >>
    rring: eat_16 >>
    lring: eat_16 >>
    feet: eat_16 >>
    misc1: eat_16 >>
    misc2: eat_16 >>
    misc3: eat_16 >>
    misc4: eat_16 >>
    machine1: eat_16 >>
    machine2: eat_16 >>
    machine3: eat_16 >>
    machine4: eat_16 >>
    spellbook: eat_16 >>
    misc5: eat_16 >>
    backpack: length_count!(eat_16, eat_16) >>
    (H3MHeroEquipment {
        head, shoulders, neck,
        rhand, lhand, torso, rring, lring, feet,
        misc1, misc2, misc3, misc4,
        machine1, machine2, machine3, machine4,
        spellbook, misc5,
        backpack,
    })
));

#[derive(Debug)]
struct H3MHeroCustomization {
    exp: Option<u32>,
    skills: Option<Vec<(u8, H3MSkillLevel)>>,
    equipment: Option<H3MHeroEquipment>,
    bio: Option<String>,
    gender: H3MHeroGender,
    spells: Option<[u8; 9]>,
    stats: Option<(u8, u8, u8, u8)>,
}

named!(eat_hero_customization<H3MHeroCustomization>, do_parse!(
    exp: eat_option!(eat_32) >>
    skills: eat_option!(length_count!(eat_32, tuple!(eat_8, eat_skill_level))) >>
    equipment: eat_option!(eat_hero_equipment) >>
    bio: eat_option!(eat_string) >>
    gender: eat_hero_gender >>
    spells: eat_option!(count_fixed!(u8, eat_8, 9)) >>
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
    terrain_type_mask1: u16,
    terrain_type_mask2: u16,
    class: u32,
    subclass: u32,
    group: u8,
    is_overlay: bool,
}

named!(eat_object_template<H3MObjectTemplate>, do_parse!(
    filename: eat_string >>
    shape_mask: count_fixed!(u8, eat_8, 6) >>
    visit_mask: count_fixed!(u8, eat_8, 6) >>
    terrain_type_mask1: eat_16 >>
    terrain_type_mask2: eat_16 >>
    class: eat_32 >>
    subclass: eat_32 >>
    group: eat_8 >>
    is_overlay: eat_flag >>
    zeroes: tag!(&[0u8; 16]) >>
    (H3MObjectTemplate {
        filename, shape_mask, visit_mask, terrain_type_mask1, terrain_type_mask2,
        class, subclass, group, is_overlay,
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

named!(eat_town_event<H3MTownEvent>, do_parse!(
    event: eat_event >>
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
    owner: u8,
    name: Option<String>,
    garrison: Option<Vec<(H3MCreature, u16)>>, // TODO: always 7 slots
    group_formation: bool,
    buildings: H3MBuildings,
    forced_spells: [u8; 9],
    allowed_spells: [u8; 9],
    events: Vec<H3MTownEvent>,
    alignment: u8,
}

named!(eat_obj_town<H3MObjectTown>, do_parse!(
    id: eat_32 >>
    owner: eat_8 >>
    name: eat_option!(eat_string) >>
    garrison: eat_option!(count!(tuple!(eat_creature, eat_16), 7)) >>
    group_formation: eat_flag >>
    buildings: eat_buildings >>
    forced_spells: count_fixed!(u8, eat_8, 9) >>
    allowed_spells: count_fixed!(u8, eat_8, 9) >>
    events: length_count!(eat_32, eat_town_event) >>
    alignment: eat_8 >>
    zeroes: tag!([0u8; 3]) >>
    (H3MObjectTown{
        id, owner, name, garrison, group_formation, buildings,
        forced_spells, allowed_spells, events, alignment
    })
));

#[derive(Debug)]
enum H3MObjectSettings {
    Unimplemented,
    RandomTown(H3MObjectTown),
    Town(H3MObjectTown),
}

named_args!(eat_object_settings(class: u32)<H3MObjectSettings>,
            switch!(
                map!(value!(class), |c| { println!("class: {}", c); c }),
                77 => map!(eat_obj_town, |t| H3MObjectSettings::RandomTown(t)) |
                98 => map!(eat_obj_town, |t| H3MObjectSettings::Town(t)) |
                _ => value!(H3MObjectSettings::Unimplemented, dbg!(tag!("unsupported object class")))
            )
);

#[derive(Debug)]
struct H3MObject {
    loc: H3MLocation,
    id: u32,
    settings: H3MObjectSettings,
}

named_args!(eat_object<'a>(templates: &'a[H3MObjectTemplate])<H3MObject>, do_parse!(
    loc: eat_location >>
    id: eat_32 >>
    _zeroes: tag!([0u8; 5]) >>
    settings: call!(eat_object_settings, templates[id as usize].class) >>
    (H3MObject {
        loc, id, settings
    })
));

#[derive(Debug)]
struct H3MEvent {
    name: String,
    text: String,
    resources: [u32; 7],
    todo: [u8; 3],
    first_occurence: u16,
    repeat_period: u16,
}

named!(eat_event<H3MEvent>, do_parse!(
    name: eat_string >>
    text: eat_string >>
    resources: count_fixed!(u32, eat_32, 7) >>
    todo: count_fixed!(u8, eat_8, 3) >>
    first_occurence: eat_16 >>
    repeat_period: eat_16 >>
    zeroes: tag!([0u8; 16]) >>
    (H3MEvent {
        name, text, resources, todo, first_occurence, repeat_period
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MFile {
    header: H3MHeader,
    players: [H3MPlayer; 8],
    victory: Option<H3MSpecialVictoryCondition>,
    loss: H3MLossCondition,
    teams: Option<[u8; 8]>,
    available_heroes: H3MAvailableHeroes,
    artifacts: [u8; 18],
    spells: [u8; 9],
    skills: [u8; 4],
    rumors: Vec<(String, String)>,
    heroes: Vec<Option<H3MHeroCustomization>>, // TODO: this always has 156 items
    land: H3MMap,
    underground: Option<H3MMap>,
    object_templates: Vec<H3MObjectTemplate>,
    objects: Vec<H3MObject>,
    //events: Vec<H3MEvent>,
}

named!(eat_h3m<H3MFile>, do_parse!(
    header: eat_header >>
    p0: eat_player >> p1: eat_player >> p2: eat_player >> p3: eat_player >>
    p4: eat_player >> p5: eat_player >> p6: eat_player >> p7: eat_player >>
    victory: eat_special_victory >>
    loss: eat_loss >>
    teams: switch!(eat_8,
        0u8 => value!(None) |
        _ => map!(count_fixed!(u8, eat_8, 8), |x| Some(x))
    ) >>
    available_heroes: eat_available_heroes >>
    _zeroes: tag!([0u8; 31]) >>
    artifacts: count_fixed!(u8, eat_8, 18) >>
    spells: count_fixed!(u8, eat_8, 9) >>
    skills: count_fixed!(u8, eat_8, 4) >>
    rumors: length_count!(eat_32, tuple!(eat_string, eat_string)) >>
    heroes: count!(eat_option!(eat_hero_customization), 156) >>
    land: count!(eat_tile, header.get_width() * header.get_height()) >>
    underground: switch!(value!(header.has_underground),
        false => value!(None) |
        true => map!(count!(eat_tile, header.get_width() * header.get_height()), |x| Some(x))
    ) >>
    object_templates: length_count!(eat_32, eat_object_template) >>
    objects: length_count!(map!(eat_32, |n| n.min(1)), call!(eat_object, &object_templates)) >>
    //events: length_count!(eat_32, eat_event) >>
    //_trailing_zeroes: count!(tag!([0u8]), 124) >>
    (H3MFile {
        header,
        players: [p0, p1, p2, p3, p4, p5, p6, p7],
        victory, loss, teams, available_heroes,
        artifacts, spells, skills, rumors, heroes,
        land: H3MMap { tiles: land },
        underground: underground.map(|tiles| H3MMap { tiles }),
        object_templates, objects, //events
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

fn main() {
    let res = File::open("rust.h3m").and_then(|f| {
        let br = BufReader::new(f);
        let mut buf: Vec<u8> = Vec::new();
        GzDecoder::new(br).read_to_end(&mut buf).map(move |_| buf)
    });

    match res {
        Ok(bin) => {
            println!("unzipped size: {}", bin.len());

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
                nom::IResult::Done(rem, doc) => {
                    println!("parsed document: {:#?}", doc);

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
                    println!("remaining: {:?}", rem.len());
                }
                nom::IResult::Error(e) => println!("error: {:#?}", e),
                nom::IResult::Incomplete(n) => println!("need: {:#?}", n),
            }
        },
        Err(err) => println!("error: {}", err),
    }
}
