extern crate flate2;

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

macro_rules! eat_vector_8 (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        do_parse!($i,
            n: eat_8 >>
            xs: count!($submac!($($args)*), n as usize) >>
            (xs)
        )
    );
    ($i:expr, $f:expr) => (
        eat_vector_8!($i, call!($f))
    );
);

macro_rules! eat_vector_16 (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        do_parse!($i,
            n: eat_16 >>
            xs: count!($submac!($($args)*), n as usize) >>
            (xs)
        )
    );
    ($i:expr, $f:expr) => (
        eat_vector_16!($i, call!($f))
    );
);

macro_rules! eat_vector_32 (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        do_parse!($i,
            n: eat_32 >>
            xs: count!($submac!($($args)*), n as usize) >>
            (xs)
        )
    );
    ($i:expr, $f:expr) => (
        eat_vector_32!($i, call!($f))
    );
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MVersion { RoE, AB, SoD }

named!(eat_version<H3MVersion>, switch!(eat_8,
    0x0E => value!(H3MVersion::RoE, tag!(b"\0\0\0")) |
    0x15 => value!(H3MVersion::AB, tag!(b"\0\0\0")) |
    0x1C => value!(H3MVersion::SoD, tag!(b"\0\0\0"))
));

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

#[derive(Debug)]
enum H3MSize { S, M, L, XL }

named!(eat_size<H3MSize>, switch!(eat_32,
    36 => value!(H3MSize::S) |
    72 => value!(H3MSize::M) |
    108 => value!(H3MSize::L) |
    144 => value!(H3MSize::XL)
));

impl H3MSize {
    fn tiles(&self) -> usize {
        match *self {
            H3MSize::S => 36,
            H3MSize::M => 72,
            H3MSize::L => 108,
            H3MSize::XL => 144,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MDifficulty { Easy, Normal, Hard, Expert, Impossible }

named!(eat_difficulty<H3MDifficulty>, switch!(eat_8,
    0 => value!(H3MDifficulty::Easy) |
    1 => value!(H3MDifficulty::Normal) |
    2 => value!(H3MDifficulty::Hard) |
    3 => value!(H3MDifficulty::Expert) |
    4 => value!(H3MDifficulty::Impossible)
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MHeader {
    version: H3MVersion,
    unknown: u8,
    size: H3MSize,
    has_underground: bool,
    name: String,
    description: String,
    difficulty: H3MDifficulty,
    level_cap: u8, // TODO: AB/SoD only?
}

impl H3MHeader {
    fn get_width(&self) -> usize {
        self.size.tiles()
    }
    fn get_height(&self) -> usize {
        self.size.tiles()
    }
}

named!(eat_header<H3MHeader>, do_parse!(
    version: eat_version >>
    unknown: eat_8 >>
    size: eat_size >>
    has_underground: eat_flag >>
    name: eat_string >>
    description: eat_string >>
    difficulty: eat_difficulty >>
    level_cap: eat_8 >>
    (H3MHeader {
        version,
        unknown,
        size,
        has_underground,
        name,
        description,
        difficulty,
        level_cap,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MTownKind {
    Castle, Rampart, Tower,
    Inferno, Necropolis, Dungeon,
    Stronghold, Fortress, Conflux,
    Random,
}

named!(eat_town_kind<H3MTownKind>, switch!(eat_8,
    0x00 => value!(H3MTownKind::Castle) |
    0x01 => value!(H3MTownKind::Rampart) |
    0x02 => value!(H3MTownKind::Tower) |
    0x03 => value!(H3MTownKind::Inferno) |
    0x04 => value!(H3MTownKind::Necropolis) |
    0x05 => value!(H3MTownKind::Dungeon) |
    0x06 => value!(H3MTownKind::Stronghold) |
    0x07 => value!(H3MTownKind::Fortress) |
    0x08 => value!(H3MTownKind::Conflux) |
    0xFF => value!(H3MTownKind::Random)
));

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

#[derive(Debug)]
enum H3MPlayerBehavior {
    Random, Warrior, Builder, Explorer,
}

named!(eat_player_behavior<H3MPlayerBehavior>, switch!(eat_8,
    0 => value!(H3MPlayerBehavior::Random) |
    1 => value!(H3MPlayerBehavior::Warrior) |
    2 => value!(H3MPlayerBehavior::Builder) |
    3 => value!(H3MPlayerBehavior::Explorer)
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MArtifact(u8);

named!(artifact<H3MArtifact>, map!(eat_8, |i| H3MArtifact(i)));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MResource {
    Wood, Mercury, Ore, Sulfur, Crystals, Gems, Gold,
}

named!(resource<H3MResource>, switch!(eat_8,
    0 => value!(H3MResource::Wood) |
    1 => value!(H3MResource::Mercury) |
    2 => value!(H3MResource::Ore) |
    3 => value!(H3MResource::Sulfur) |
    4 => value!(H3MResource::Crystals) |
    5 => value!(H3MResource::Gems) |
    6 => value!(H3MResource::Gold)
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MHallLevel {
    Town, City, Capitol,
}

named!(hall_level<H3MHallLevel>, switch!(eat_8,
    0 => value!(H3MHallLevel::Town) |
    1 => value!(H3MHallLevel::City) |
    2 => value!(H3MHallLevel::Capitol)
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MCastleLevel {
    Fort, Citadel, Castle,
}

named!(castle_level<H3MCastleLevel>, switch!(eat_8,
    0 => value!(H3MCastleLevel::Fort) |
    1 => value!(H3MCastleLevel::Citadel) |
    2 => value!(H3MCastleLevel::Castle)
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MCreature(u16);

named!(creature<H3MCreature>, map!(eat_16, |i| H3MCreature(i)));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MVictoryCondition {
    None,
    AcquireArtifact(bool, bool, H3MArtifact),
    AccumCreatures(bool, bool, H3MCreature, u32),
    AccumResources(bool, bool, H3MResource, u32),
    UpgradeTown(bool, bool, H3MLocation, H3MHallLevel, H3MCastleLevel),
    BuildGrail(bool, bool, H3MLocation),
    DefeatHero(bool, bool, H3MLocation),
    CaptureTown(bool, bool, H3MLocation),
    DefeatMoster(bool, bool, H3MLocation),
    FlagAllDwellings(bool, bool),
    FlagAllMines(bool, bool),
    TransportArtifact(bool, bool, H3MArtifact, H3MLocation),
}

named!(eat_victory<H3MVictoryCondition>,
       switch!(eat_8,
               0xFF => value!(H3MVictoryCondition::None) |
               0x00 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 art: artifact >>
                                 (H3MVictoryCondition::AcquireArtifact(def, cpu, art))) |
               0x01 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 cr: creature >> amount: eat_32 >>
                                 (H3MVictoryCondition::AccumCreatures(def, cpu, cr, amount))) |
               0x02 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 res: resource >> amount: eat_32 >>
                                 (H3MVictoryCondition::AccumResources(def, cpu, res, amount))) |
               0x03 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 loc: eat_location >>
                                 hall: hall_level >> castle: castle_level >>
                                 (H3MVictoryCondition::UpgradeTown(def, cpu, loc, hall, castle))) |
               0x04 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 loc: eat_location >>
                                 (H3MVictoryCondition::BuildGrail(def, cpu, loc))) |
               0x05 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 loc: eat_location >>
                                 (H3MVictoryCondition::DefeatHero(def, cpu, loc))) |
               0x06 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 loc: eat_location >>
                                 (H3MVictoryCondition::CaptureTown(def, cpu, loc))) |
               0x07 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 loc: eat_location >>
                                 (H3MVictoryCondition::DefeatMoster(def, cpu, loc))) |
               0x08 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 (H3MVictoryCondition::FlagAllDwellings(def, cpu))) |
               0x09 => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 (H3MVictoryCondition::FlagAllMines(def, cpu))) |
               0x0A => do_parse!(def: eat_flag >> cpu: eat_flag >>
                                 art: artifact >> loc: eat_location >>
                                 (H3MVictoryCondition::TransportArtifact(def, cpu, art, loc)))
       )
);

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
enum H3MColor {
    Red, Blue, Tan, Green, Orange, Purple, Teal, Pink,
}

#[derive(Debug)]
struct H3MPlayer {
    color: H3MColor, // convenience
    playability: H3MPlayerPlayability,
    allowed_alignments: H3MPlayerAllowedAlignments,
    main_town: Option<H3MMainTown>,
    random_hero: bool,
    hero_type: u8,
    main_hero: Option<H3MHero>,
    unknown: u8, // TODO: check if this is related to `placeholders`
    heroes: Vec<H3MHero>,
}

named_args!(eat_player(color: H3MColor)<H3MPlayer>, do_parse!(
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
    unknown: eat_8 >>
    heroes: eat_vector_32!(eat_hero) >>
    (H3MPlayer {
        color,
        playability,
        allowed_alignments,
        main_town,
        random_hero,
        hero_type,
        main_hero,
        unknown,
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
    unknown1: [u8; 4],
    settings: Vec<H3MHeroAvailability>,
    unknown2: [u8; 31],
}

named!(eat_available_heroes<H3MAvailableHeroes>, do_parse!(
    mask: count_fixed!(u8, eat_8, 20) >>
    unknown1: count_fixed!(u8, eat_8, 4) >>
    settings: eat_vector_8!(eat_hero_availability) >>
    unknown2: count_fixed!(u8, eat_8, 31) >>
    (H3MAvailableHeroes {
        mask, unknown1, settings, unknown2,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MSkillLevel {
    Basic, Advanced, Expert,
}

named!(eat_skill_level<H3MSkillLevel>, switch!(eat_8,
    0x00 => value!(H3MSkillLevel::Basic) |
    0x01 => value!(H3MSkillLevel::Advanced) |
    0x02 => value!(H3MSkillLevel::Expert)
));

#[derive(Debug)]
enum H3MHeroGender {
    Default, Male, Female,
}

named!(eat_hero_gender<H3MHeroGender>, switch!(eat_8,
    0x00 => value!(H3MHeroGender::Male) |
    0x01 => value!(H3MHeroGender::Female) |
    0xFF => value!(H3MHeroGender::Default)
));

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
    backpack: eat_vector_16!(eat_16) >>
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
    skills: eat_option!(eat_vector_32!(tuple!(eat_8, eat_skill_level))) >>
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

#[derive(Debug)]
enum H3MTerrainType {
    Dirt, Sand, Grass, Snow, Swamp, Rough, Subterranean, Lava, Water, Rock
}

named!(eat_terrain_type<H3MTerrainType>,
       switch!(eat_8,
               0x00 => value!(H3MTerrainType::Dirt) |
               0x01 => value!(H3MTerrainType::Sand) |
               0x02 => value!(H3MTerrainType::Grass) |
               0x03 => value!(H3MTerrainType::Snow) |
               0x04 => value!(H3MTerrainType::Swamp) |
               0x05 => value!(H3MTerrainType::Rough) |
               0x06 => value!(H3MTerrainType::Subterranean) |
               0x07 => value!(H3MTerrainType::Lava) |
               0x08 => value!(H3MTerrainType::Water) |
               0x09 => value!(H3MTerrainType::Rock)
       )
);

impl H3MTerrainType {
    fn to_symbol(&self) -> u8 {
        match *self {
            H3MTerrainType::Dirt => b':',
            H3MTerrainType::Sand => b'_',
            H3MTerrainType::Grass => b'v',
            H3MTerrainType::Snow => b'*',
            H3MTerrainType::Swamp => b'.',
            H3MTerrainType::Rough => b'#',
            H3MTerrainType::Subterranean => b'%',
            H3MTerrainType::Lava => b'x',
            H3MTerrainType::Water => b'~',
            H3MTerrainType::Rock => b'^',
        }
    }
}

#[derive(Debug)]
enum H3MRiverType {
    None, Clear, Icy, Muddy, Lava
}

named!(eat_river_type<H3MRiverType>,
       switch!(eat_8,
               0x00 => value!(H3MRiverType::None) |
               0x01 => value!(H3MRiverType::Clear) |
               0x02 => value!(H3MRiverType::Icy) |
               0x03 => value!(H3MRiverType::Muddy) |
               0x04 => value!(H3MRiverType::Lava)
       )
);

#[derive(Debug)]
enum H3MRiverTopology {
    Turn1, Turn2, Turn3, Turn4,
    Cross,
    TVert1, TVert2,
    THorz1, THorz2,
    Vert1, Vert2,
    Horz1, Horz2
}

named!(eat_river_topo<H3MRiverTopology>,
       switch!(eat_8,
               0x00 => value!(H3MRiverTopology::Turn1) |
               0x01 => value!(H3MRiverTopology::Turn2) |
               0x02 => value!(H3MRiverTopology::Turn3) |
               0x03 => value!(H3MRiverTopology::Turn4) |
               0x04 => value!(H3MRiverTopology::Cross) |
               0x05 => value!(H3MRiverTopology::TVert1) |
               0x06 => value!(H3MRiverTopology::TVert2) |
               0x07 => value!(H3MRiverTopology::THorz1) |
               0x08 => value!(H3MRiverTopology::THorz2) |
               0x09 => value!(H3MRiverTopology::Vert1) |
               0x0A => value!(H3MRiverTopology::Vert2) |
               0x0B => value!(H3MRiverTopology::Horz1) |
               0x0C => value!(H3MRiverTopology::Horz2)
       )
);

#[derive(Debug)]
enum H3MRoadType {
    None, Dirt, Gravel, Cobblestone
}

named!(eat_road_type<H3MRoadType>,
       switch!(eat_8,
               0x00 => value!(H3MRoadType::None) |
               0x01 => value!(H3MRoadType::Dirt) |
               0x02 => value!(H3MRoadType::Gravel) |
               0x03 => value!(H3MRoadType::Cobblestone)
       )
);

#[derive(Debug)]
enum H3MRoadTopology {
    Turn1, Turn2, Turn3, Turn4, Turn5, Turn6,
    TVert1, TVert2,
    THorz1, THorz2,
    Vert1, Vert2,
    Horz1, Horz2,
    EndVert, EndHorz,
    Cross
}

named!(eat_road_topo<H3MRoadTopology>,
       switch!(eat_8,
               0x00 => value!(H3MRoadTopology::Turn1) |
               0x01 => value!(H3MRoadTopology::Turn2) |
               0x02 => value!(H3MRoadTopology::Turn3) |
               0x03 => value!(H3MRoadTopology::Turn4) |
               0x04 => value!(H3MRoadTopology::Turn5) |
               0x05 => value!(H3MRoadTopology::Turn6) |
               0x06 => value!(H3MRoadTopology::TVert1) |
               0x07 => value!(H3MRoadTopology::TVert2) |
               0x08 => value!(H3MRoadTopology::THorz1) |
               0x09 => value!(H3MRoadTopology::THorz2) |
               0x0A => value!(H3MRoadTopology::Vert1) |
               0x0B => value!(H3MRoadTopology::Vert2) |
               0x0C => value!(H3MRoadTopology::Horz1) |
               0x0D => value!(H3MRoadTopology::Horz2) |
               0x0E => value!(H3MRoadTopology::EndVert) |
               0x0F => value!(H3MRoadTopology::EndHorz) |
               0x10 => value!(H3MRoadTopology::Cross)
       )
);

impl H3MRoadTopology {
    fn to_symbol(&self) -> u8 {
        match *self {
            H3MRoadTopology::Turn1 => b'1',
            H3MRoadTopology::Turn2 => b'2',
            H3MRoadTopology::Turn3 => b'3',
            H3MRoadTopology::Turn4 => b'4',
            H3MRoadTopology::Turn5 => b'5',
            H3MRoadTopology::Turn6 => b'6',
            H3MRoadTopology::TVert1 => b'>',
            H3MRoadTopology::TVert2 => b'<',
            H3MRoadTopology::THorz1 => b'^',
            H3MRoadTopology::THorz2 => b'v',
            H3MRoadTopology::Vert1 => b'I',
            H3MRoadTopology::Vert2 => b'|',
            H3MRoadTopology::Horz1 => b'-',
            H3MRoadTopology::Horz2 => b'~',
            H3MRoadTopology::EndVert => b'*',
            H3MRoadTopology::EndHorz => b'o',
            H3MRoadTopology::Cross => b'+',
        }
    }
}

#[derive(Debug)]
struct H3MTile {
    terrain: H3MTerrainType,
    texture: u8,
    river_type: H3MRiverType,
    river_topo: H3MRiverTopology,
    road_type: H3MRoadType,
    road_topo: H3MRoadTopology,
    mirror: u8,
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
        mirror
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MFile {
    header: H3MHeader,
    players: [H3MPlayer; 8],
    victory: H3MVictoryCondition,
    loss: H3MLossCondition,
    teams: Option<[u8; 8]>,
    available_heroes: H3MAvailableHeroes,
    artifacts: [u8; 18],
    spells: [u8; 9],
    skills: [u8; 4],
    rumors: Vec<(String, String)>,
    heroes: Vec<Option<H3MHeroCustomization>>, // TODO: this always has 156 items
    land: Vec<H3MTile>,
/*
    underground: Option<Vec<H3MTile>>,
    objects: Vec<H3MObject>,
*/
}

named!(eat_h3m<H3MFile>, do_parse!(
    header: eat_header >>
    p0: call!(eat_player, H3MColor::Red) >>
    p1: call!(eat_player, H3MColor::Blue) >>
    p2: call!(eat_player, H3MColor::Tan) >>
    p3: call!(eat_player, H3MColor::Green) >>
    p4: call!(eat_player, H3MColor::Orange) >>
    p5: call!(eat_player, H3MColor::Purple) >>
    p6: call!(eat_player, H3MColor::Teal) >>
    p7: call!(eat_player, H3MColor::Pink) >>
    victory: eat_victory >>
    loss: eat_loss >>
    teams: switch!(eat_8,
        0u8 => value!(None) |
        _ => map!(count_fixed!(u8, eat_8, 8), |x| Some(x))
    ) >>
    available_heroes: eat_available_heroes >>
    artifacts: count_fixed!(u8, eat_8, 18) >>
    spells: count_fixed!(u8, eat_8, 9) >>
    skills: count_fixed!(u8, eat_8, 4) >>
    rumors: eat_vector_32!(tuple!(eat_string, eat_string)) >>
    heroes: count!(eat_option!(eat_hero_customization), 156) >>
    land: count!(eat_tile, header.get_width() * header.get_height()) >>
/*
        underground: switch!(value!(header.has_underground),
        false => value!(None) |
        true => map!(count!(eat_tile, header.get_width() * header.get_height()))
    ) >>
    objects: eat_vector_32!(eat_object) >>
*/
    (H3MFile {
        header,
        players: [p0, p1, p2, p3, p4, p5, p6, p7],
        victory, loss, teams, available_heroes,
        artifacts, spells, skills, rumors, heroes,
        land, //underground, objects,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

use std::fmt::Write;

fn main() {
    let res = File::open("rust.h3m").and_then(|f| {
        let br = BufReader::new(f);
        let mut buf: Vec<u8> = Vec::new();
        GzDecoder::new(br).read_to_end(&mut buf).map(move |_| buf)
    });

    match res {
        Ok(bin) => {
            println!("unzipped size: {}", bin.len());
            
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

            match eat_h3m(&bin) {
                nom::IResult::Done(_, doc) => {
                    println!("parsed document: {:#?}", doc);
                    let w = doc.header.get_width();
                    let h = doc.header.get_height();
                    let mut terrain = String::new();
                    for r in 0..h {
                        for c in 0..w {
                            terrain.push(doc.land[r * w + c].terrain.to_symbol() as char);
                        }
                        terrain.push('\n');
                    }
                    println!("terrain:\n{}\n", terrain);
                    terrain.clear();
                    for r in 0..h {
                        for c in 0..w {
                            let tile = &doc.land[r * w + c];
                            if let H3MRoadType::None = tile.road_type {
                                terrain.push('.');
                            } else {
                                terrain.push(tile.road_topo.to_symbol() as char);
                            }
                        }
                        terrain.push('\n');
                    }
                    println!("road_topo:\n{}\n", terrain);
                }
                nom::IResult::Error(e) => println!("error: {:#?}", e),
                nom::IResult::Incomplete(n) => println!("need: {:#?}", n),
            }
        },
        Err(err) => println!("error: {}", err),
    }
}
