use Eat;
use Put;

macro_rules! h3m_enum_derive {
    ( [$($d:ident),*] <$t:ty, $u:ident, $f:ident, $p:ident> $( ($x:expr, $y:ident) )* ) => (
        #[derive( $($d),* )]
        pub enum $u {
            $( $y, )*
        }

        impl $u {
            fn read(x: $t) -> Option<$u> {
                match x {
                    $( $x => Some($u::$y), )*
                        _ => panic!("{}: unexpected {}", x, stringify!($u))
                    //  _ => None
                }
            }

            #[cfg(feature = "put")]
            #[allow(dead_code)]
            fn write(&self) -> $t {
                match *self {
                    $( $u::$y => $x, )*
                }
            }
        }

        impl Eat {
            named_attr!(#[allow(unused_imports)], pub $f<$u>, do_parse!(x: call!(Eat::$p) >> y: expr_opt!($u::read(x)) >> (y)));
        }

        #[cfg(feature = "put")]
        #[allow(dead_code)]
        impl Put {
            pub fn $f(o: &mut Vec<u8>, v: &$u) -> bool
            {
                let ret = Put::$p(o, &v.write());
                ret
            }
        }
    );
}

macro_rules! h3m_enum {
    ( <$t:ty, $u:ident, $f:ident, $p:ident> $( ($x:expr, $y:ident) )* ) => (
        h3m_enum_derive! { [Debug] <$t, $u, $f, $p> $( ($x, $y) )* }
    );
}

h3m_enum_derive! { [Debug, Clone, Copy] <u32, H3MObjectClass, obj_class, long>

    (2, AltarOfSacrifice)

    (4, Arena)
    (5, Artifact) //
    (6, PandorasBox) //
    (7, BlackMarket)
    (8, Boat)
    (9, BorderGuard)
    (10, KeymastersTent)
    (11, Buoy)
    (12, Campfire)
    (13, Cartographer)
    (14, SwanPond)
    (15, CoverOfDarkness)
    (16, CreatureBank)
    (17, CreatureGenerator1) //
    (18, CreatureGenerator2) //
    (19, CreatureGenerator3) //
    (20, CreatureGenerator4) //
    (21, CursedGround1)
    (22, Corpse)
    (23, MarlettoTower)
    (24, DerelictShip)
    (25, DragonUtopia)
    (26, Event) //
    (27, EyeOfMagi)
    (28, FaerieRing)
    (29, Flotsam)
    (30, FountainOfFortune)
    (31, FountainOfYouth)
    (32, GardenOfRevelation)
    (33, Garrison) //
    (34, Hero) //
    (35, HillFort)
    (36, Grail) //
    (37, HutOfMagi)
    (38, IdolOfFortune)
    (39, LeanTo)

    (41, LibraryOfEnlightenment)
    (42, Lighthouse) //
    (43, MonolithEntrance)
    (44, MonolithExit)
    (45, MonolithTwoWay)
    (46, MagicPlains1)
    (47, SchoolOfMagic)
    (48, MagicSpring)
    (49, MagicWell)
    (50, MarketOfTime)
    (51, MercenaryCamp)
    (52, Mermaid)
    (53, Mine) //
    (54, Monster) //
    (55, MysticalGarden)
    (56, Oasis)
    (57, Obelisk)
    (58, RedwoodObservatory)
    (59, OceanBottle) //
    (60, PillarOfFire)
    (61, StarAxis)
    (62, Prison) //
    (63, Pyramid)
    (64, RallyFlag)
    (65, RandomArtifact) //
    (66, RandomTreasureArtifact) //
    (67, RandomMinorArtifact) //
    (68, RandomMajorArtifact) //
    (69, RandomRelicArtifact) //
    (70, RandomHero) //
    (71, RandomMonster) //
    (72, RandomMonster1) //
    (73, RandomMonster2) //
    (74, RandomMonster3) //
    (75, RandomMonster4) //
    (76, RandomResource) //
    (77, RandomTown) //
    (78, RefugeeCamp)
    (79, Resource) //
    (80, Sanctuary)
    (81, Scholar) //
    (82, SeaChest)
    (84, Crypt)
    (83, SeerHut) //
    (85, Shipwreck)
    (86, ShipwreckSurvivor)
    (87, Shipyard) //
    (88, ShrineOfMagicIncantation) // level 1 spells
    (89, ShrineOfMagicGesture) // level 2 spells
    (90, ShrineOfMagicThought) // level 3 spells
    (91, Sign) //
    (92, Sirens)
    (93, SpellScroll) //
    (94, Stables)
    (95, Tavern)
    (96, Temple)
    (97, DenOfThieves)
    (98, Town) //
    (99, TradingPost)
    (100, LearningStone)
    (101, TreasureChest)
    (102, TreeOfKnowledge)
    (103, SubterraneanGate)
    (104, University)
    (105, Wagon)
    (106, WarMachineFactory)
    (107, SchoolOfWar)
    (108, WarriorsTomb)
    (109, WaterWheel)
    (110, WateringHole)
    (111, Whirlpool)
    (112, Windmill)
    (113, WitchHut) //

    (116, Cactus)
    (117, Canyon)
    (118, Crater)
    (119, DeadVegetation)
    (120, Flowers)
    (121, FrozenLake)

    (124, Hole)
    (125, Kelp)
    (126, Lake)
    (127, LavaFlow)
    (128, LavaLake)
    (129, Mushrooms)
    (130, Log)
    (131, Mandrake)
    (132, Moss)
    (133, Mound)
    (134, Mountain)
    (135, OakTrees)
    (136, Outcropping)
    (137, PineTrees)

    (143, RiverDelta)

    (147, Rock)
    (148, SandDune)
    (149, SandPit)
    (150, Shrub)
    (151, Skull)

    (153, Stump)

    (155, Trees)

    (158, Volcano)

    (161, Reef)
    (162, RandomMonster5) //
    (163, RandomMonster6) //
    (164, RandomMonster7) //

    (177, Lake2)

    (199, Trees2)

    (206, DesertHills)
    (207, DirtHills)
    (208, GrassHills)
    (209, RoughHills)
    (210, SubterraneanRocks)
    (211, SwampFoliage)
    (212, BorderGate)
    (213, FreelancersGuild)
    (214, HeroPlaceholder) //
    (215, QuestGuard) //
    (216, RandomDwelling) //
    (217, RandomDwellingLevel) //
    (218, RandomDwellingFaction) //
    (219, Garrison2) //
    (220, AbandonedMine) //
    (221, TradingPostSnow)
    (222, Cloverfield)
    (223, CursedGround2)
    (224, EvilFog)
    (225, FavorableWinds)
    (226, FieryFields)
    (227, HolyGrounds)
    (228, LucidPools)
    (229, MagicClouds)
    (230, MagicPlains2)
    (231, RockLands)
}

h3m_enum_derive! { [Debug, Clone, Copy] <u32, H3MVersion, version, long>
    (0x0000000E, RoE)
    (0x00000015, AB)
    (0x0000001C, SoD)
}

h3m_enum! { <u32, H3MSize, size, long>
    (36, S)
    (72, M)
    (108, L)
    (144, XL)
}

h3m_enum! { <u8, H3MDifficulty, difficulty, byte>
    (0, Easy)
    (1, Normal)
    (2, Hard)
    (3, Expert)
    (4, Impossible)
}

h3m_enum! { <u8, H3MSkillLevel, skill_level, byte>
    (0, Unspecified)
    (1, Basic)
    (2, Advanced)
    (3, Expert)
}

h3m_enum! { <u8, H3MHeroGender, hero_gender, byte>
    (0x00, Male)
    (0x01, Female)
    (0xFF, Default) // TODO: option?
}

h3m_enum! { <u8, H3MColor, color, byte>
    (0, Red)
    (1, Blue)
    (2, Tan)
    (3, Green)
    (4, Orange)
    (5, Purple)
    (6, Teal)
    (7, Pink)
    (0xFF, Unspecified) // TODO: option?
}

h3m_enum! { <u8, H3MStat, stat, byte>
    (0, Attack)
    (1, Defense)
    (2, SpellPower)
    (3, Knowledge)
}

h3m_enum! { <u8, H3MSkill, skill, byte>
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

h3m_enum! { <u8, H3MModifier, modifier, byte>
    (253, Minus3)
    (254, Minus2)
    (255, Minus1)
    (0, NoChange)
    (1, Plus1)
    (2, Plus2)
    (3, Plus3)
}

h3m_enum! { <u8, H3MSpell, spell, byte>
    (0, SummonBoat)
    (1, ScuttleBoat)
    (2, Vision)
    (3, ViewEarth)
    (4, Disguise)
    (5, ViewAir)
    (6, Fly)
    (7, WaterWalk)
    (8, DimensionDoor)
    (9, TownPortal)
    (10, Quicksand)
    (11, LandMine)
    (12, ForceField)
    (13, FireWall)
    (14, Earthquake)
    (15, MagicArrow)
    (16, IceBolt)
    (17, LightningBolt)
    (18, Implosion)
    (19, ChainLightning)
    (20, FrostRing)
    (21, Fireball)
    (22, Inferno)
    (23, MeteorShower)
    (24, DeathRipple)
    (25, DestroyUndead)
    (26, Armageddon)
    (27, Shield)
    (28, AirShield)
    (29, FireShield)
    (30, ProtectionFromAir)
    (31, ProtectionFromFire)
    (32, ProtectionFromWater)
    (33, ProtectionFromEarth)
    (34, Antimagic)
    (35, DispelMagic)
    (36, MagicMirror)
    (37, Cure)
    (38, Resurrection)
    (39, AnimateDead)
    (40, Sacrifice)
    (41, Bless)
    (42, Curse)
    (43, Bloodlust)
    (44, Precision)
    (45, Weakness)
    (46, StoneSkin)
    (47, DisruptionRay)
    (48, Prayer)
    (49, Sorrow)
    (50, Fortune)
    (51, Mirth)
    (52, Misfortune)
    (53, Haste)
    (54, Slow)
    (55, Slayer)
    (56, Frenzy)
    (57, Fear)
    (58, Counterstrike)
    (59, Berserk)
    (60, Hypnose)
    (61, Forgetfullnes)
    (62, Blind)
    (63, Teleport)
    (64, RemoveObstacle)
    (65, Clone)
    (66, FireElemental)
    (67, EarthElemental)
    (68, WaterElemental)
    (69, AirElemental)
}

h3m_enum! { <u8, H3MDisposition, disposition, byte>
    (0, Compliant)
    (1, Friendly)
    (2, Aggressive)
    (3, Hostile)
    (4, Savage)
}

h3m_enum! { <u8, H3MTerrainType, terrain_type, byte>
    (0x00, TrDirt)
    (0x01, TrSand)
    (0x02, TrGrass)
    (0x03, TrSnow)
    (0x04, TrSwamp)
    (0x05, TrRough)
    (0x06, TrSubterranean)
    (0x07, TrLava)
    (0x08, TrWater)
    (0x09, TrRock)
}

h3m_enum! { <u8, H3MRiverType, river_type, byte>
    (0x00, RvNone)
    (0x01, RvClear)
    (0x02, RvIcy)
    (0x03, RvMuddy)
    (0x04, RvLava)
}

h3m_enum! { <u8, H3MRiverTopology, river_topo, byte>
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

h3m_enum! { <u8, H3MRoadType, road_type, byte>
    (0x00, RdNone)
    (0x01, RdDirt)
    (0x02, RdGravel)
    (0x03, RdCobblestone)
}

h3m_enum! { <u8, H3MRoadTopology, road_topo, byte>
    (0x00, Turn1)
    (0x01, Turn2)
    (0x02, Diag1)
    (0x03, Diag2)
    (0x04, Diag3)
    (0x05, Diag4)
    (0x06, TVert1)
    (0x07, TVert2)
    (0x08, THorz1)
    (0x09, THorz2)
    (0x0A, Vert1)
    (0x0B, Vert2)
    (0x0C, Horz1)
    (0x0D, Horz2)
    (0x0E, EndVert)
    (0x0F, EndHorz)
    (0x10, Cross)
}

h3m_enum! { <u8, H3MResource, resource, byte>
    (0, Wood)
    (1, Mercury)
    (2, Ore)
    (3, Sulfur)
    (4, Crystals)
    (5, Gems)
    (6, Gold)
}

h3m_enum! { <u8, H3MHallLevel, hall_level, byte>
    (0, Town)
    (1, City)
    (2, Capitol)
}

h3m_enum! { <u8, H3MCastleLevel, castle_level, byte>
    (0, Fort)
    (1, Citadel)
    (2, Castle)
}

h3m_enum! { <u8, H3MPlayerBehavior, player_behavior, byte>
    (0, Random)
    (1, Warrior)
    (2, Builder)
    (3, Explorer)
}

h3m_enum! { <u8, H3MTownKind, town_kind, byte>
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
