extern crate clap;
extern crate flate2;
extern crate colored;
use colored::Colorize;

#[macro_use]
extern crate nom;

use std::fs::File;
use std::io::{BufReader, Read};
use flate2::bufread::GzDecoder;

mod enums;
use enums::*;

struct Eat {}

#[cfg(feature = "put")]
struct Put {}

///////////////////////////////////////////////////////////////////////////////////////////////////

impl Eat {
    named!(byte<u8>, call!(nom::le_u8));
    named!(short<u16>, call!(nom::le_u16));
    named!(long<u32>, call!(nom::le_u32));
}

#[cfg(feature = "put")]
impl Put {
    fn byte(o: &mut Vec<u8>, v: &u8) -> bool {
        o.push(*v);
        true
    }

    fn short(o: &mut Vec<u8>, v: &u16) -> bool {
        o.push(((*v >> 0) & 0xFF) as u8);
        o.push(((*v >> 8) & 0xFF) as u8);
        true
    }

    fn long(o: &mut Vec<u8>, v: &u32) -> bool {
        o.push(((*v >>  0) & 0xFF) as u8);
        o.push(((*v >>  8) & 0xFF) as u8);
        o.push(((*v >> 16) & 0xFF) as u8);
        o.push(((*v >> 24) & 0xFF) as u8);
        true
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! w_named (
    ($($t:tt)*) => (impl Eat { named!($($t)*); });
);

#[cfg(feature = "put")]
macro_rules! mon_named (
    ($f:ident<$t:ty>, $root:ident!( $($args:tt)* )) => (
        impl Put {
            fn $f(o: &mut Vec<u8>, v: &$t) -> bool {
                let ret = $root!(o, v, $($args)* );
                ret
            }
        }
    );
);

///////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! w_named_args (
    ($($t:tt)*) => (impl Eat { named_args!($($t)*); });
);

#[cfg(feature = "put")]
macro_rules! mon_named_args (
    ($f:ident( $( $a:ident : $at:ty ),* )<$t:ty>, $root:ident!( $($args:tt)* )) => (
        impl Put {
            fn $f(o: &mut Vec<u8>, v: &$t, $($a: $at),* ) -> bool {
                let ret = $root!(o, v, $($args)* );
                ret
            }
        }
    );
);

///////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! option (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        switch!($i,
            Eat::byte,
            1u8 => map!($submac!($($args)*), |x| Some(x)) |
            0u8 => value!(None)
        )
    );
    ($i:expr, $f:expr) => (
        option!($i, call!($f))
    );
);

#[cfg(feature = "put")]
macro_rules! mon_option (
    ($o:ident, $v:ident, $submac:ident!( $($args:tt)* )) => (
        mon_switch!($o, $v, mon_call!(Put::byte),
            1u8 => mon_map!($submac!($($args)*), |x| Some(ref x)) |
            0u8 => mon_value!(None)
        )
    );
    ($o:ident, $v:ident, $f:expr) => (
        mon_option!($o, $v, mon_call!($f))
    );
);

///////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! ifeq (
    ($i:expr, $c:expr, $t:pat, $thn:ident!( $($targs:tt)* ), $els:ident!( $($eargs:tt)* )) => (
        switch!($i, value!($c),
            $t => $thn!($($targs)*) |
            _ => $els!($($eargs)*)
        )
    );
);

#[cfg(feature = "put")]
macro_rules! mon_ifeq (
    ($o:ident, $v:ident, $c:expr, $t:pat, $thn:ident!( $($targs:tt)* ), $els:ident!( $($eargs:tt)* )) => (
        match $c {
            $t => $thn!($o, $v, $($targs)*),
            _ => $els!($o, $v, $($eargs)*),
        }
    );
);

///////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! sod (
    ($i:expr, $ver:expr, $old:ident!( $($old_args:tt)* ), $sod:ident!( $($sod_args:tt)* )) => (
        switch!($i, value!($ver),
            H3MVersion::SoD => $sod!($($sod_args)*) |
            _ => $old!($($old_args)*)
        )
    );
);

#[cfg(feature = "put")]
macro_rules! mon_sod (
    ($o:ident, $v:ident, $ver:expr, $old:ident!( $($old_args:tt)* ), $sod:ident!( $($sod_args:tt)* )) => (
        match $ver {
            H3MVersion::SoD => $sod!($o, $v, $($sod_args)*),
            _ => $old!($o, $v, $($old_args)*),
        }
    );
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(feature = "put")]
macro_rules! mon_switch (
    (__impl $o:ident, $v:ident, $buf:ident, $tag:ident!( $($targs:tt)* ), ) => (
        unreachable!()
    );

    (__impl $o:ident, $v:ident, $buf:ident, $tag:ident!( $($targs:tt)* ), | $x:expr => $arm:ident!( $($aargs:tt)* ) $($tail:tt)* ) => (
        {
            if $arm!($buf, $v, $($aargs)*) {
                let var = &$x;
                $tag!($o, var, $($targs)* );
                $o.extend_from_slice($buf);
                true
            } else {
                $buf.clear();
                mon_switch!(__impl $o, $v, $buf, $tag!($($targs)*), $($tail)*)
            }
        }
    );

    ($o:ident, $v:ident, $tag:ident!( $($args:tt)* ), $($list:tt)* ) => (
        {
            let tempo = &mut Vec::new();
            mon_switch!(__impl $o, $v, tempo, $tag!($($args)*), | $($list)* )
        }
    );

    ($o:ident, $v:ident, $tag:expr, $($list:tt)* ) => (
        mon_switch!($o, $v, mon_call!($tag), $($list)* )
    );
);

#[cfg(feature = "put")]
macro_rules! mon_alt (
    ($o:ident, $v:ident, mon_tag!($tag:expr) => { |_| None } | $case:ident!( $($args:tt)* ) => { |$x:ident| $pat:pat }) => (
        match *$v {
            None => { let _t = &$tag; mon_tag!($o, _t, $tag) },
            $pat => { let val = $x; $case!($o, val, $($args)*) },
        }
    );
);

#[cfg(feature = "put")]
macro_rules! mon_tag (
    ($o:ident, $ignore:expr, $tag:expr) => (
        { $o.extend_from_slice(&$tag); true }
    );
);

#[cfg(feature = "put")]
macro_rules! mon_take (
    ($o:ident, $v:ident, $n:expr) => (
        {
            let n = $n;
            if $v.len() < n { false } else {
                $o.extend_from_slice(&$v[0..n]);
                true
            }
        }
    );
);

#[cfg(feature = "put")]
macro_rules! mon_map (
    ($o:ident, $v:ident, $f:ident!( $($args:tt)* ), |$mi:ident| $pat:pat) => (
        match *$v {
            $pat => $f!($o, $mi, $($args)* ),
            _ => false,
        }
    );
    ($o:ident, $v:ident, $f:expr, |$mi:ident| $pat:pat) => (
        mon_map!($o, $v, mon_call!($f), |$mi| $pat);
    );
);

#[cfg(feature = "put")]
macro_rules! mon_do_parse (
    (__impl $stack:ident, $res:expr, ) => ( $res );
    (__impl $stack:ident, $res:expr,  $ign:ident : mon_forget!( $var:ident, $def:expr ) >>
                                   $(   $x:ident :   $y:ident!( $($z:tt)*             ) >> )* ) => (
        {
            let ref mut $var = $def;
            mon_do_parse!(__impl $stack, $res, $( $x : $y!( $($z)* ) >> )* )
        }
    );
    (__impl $stack:ident, $res:expr,  $var:ident : $head:ident!( $($args:tt)* ) >>
                                   $(   $x:ident :    $y:ident!( $(   $z:tt)* ) >> )* ) => (
        {
            let mut tempo = Vec::new();
            $res = $res && { let out = &mut tempo; $head!(out, $var, $($args)* ) };
            $stack.push(tempo);
            mon_do_parse!(__impl $stack, $res, $( $x : $y!( $($z)* ) >> )* )
        }
    );
    (__rev $stack:ident, ^ $( $rx:ident : $ry:ident!( $($rz:tt)* ) >> )* ) => (
        {
            let mut res = true;
            mon_do_parse!(__impl $stack, res, $( $rx : $ry!( $($rz)* ) >> )* )
        }
    );
    (__rev $stack:ident, $hx:ident : $hy:ident!( $($hz:tt)* ) >>
                      $( $tx:ident : $ty:ident!( $($tz:tt)* ) >> )*
                    ^ $( $rx:ident : $ry:ident!( $($rz:tt)* ) >> )* ) => (
        mon_do_parse!(__rev $stack, $( $tx : $ty!( $($tz)* ) >> )*
                                     ^ $hx : $hy!( $($hz)* ) >>
                                    $( $rx : $ry!( $($rz)* ) >> )* )
    );
    ($o:ident, $v:ident,    $( $x:ident : $y:ident!( $($z:tt)* ) >> )*   ( $pat:pat ) ) => (
        match *$v {
            $pat => {
                let mut stack = Vec::new();
                let res = mon_do_parse!(__rev stack, $( $x : $y!( $($z)* ) >> )* ^ );
                if res {
                    stack.reverse();
                    for x in stack {
                        $o.extend_from_slice(&x);
                    }
                }
                res
            },
            _ => false,
        }
    );
);

macro_rules! variable (
    ($i:expr, $v:ident) => (
        value!($i, $v)
    );
);

#[cfg(feature = "put")]
macro_rules! mon_variable (
    ($o:ident, $v:ident, $var:ident) => (
        *$var = *$v;
    );
);

macro_rules! forget (
    ($i:expr, $v:ident, $x:expr) => (
        value!($i, ())
    );
);

#[cfg(feature = "put")]
macro_rules! mon_call (
    ($o:ident, $v:ident, $f:expr $(,$arg:expr)*) => (
        $f($o, $v $(,$arg)*)
    );
);

#[cfg(feature = "put")]
macro_rules! mon_count (
    ($o:ident, $v:ident, $submac:ident!( $($args:tt)* ), $n:expr) => (
        {
            if $v.len() == $n {
                let mut res = true;
                for i in 0..$n {
                    let v = &$v[i];
                    res = res && $submac!($o, v, $($args)*);
                }
                res
            } else { false }
        }
    );
);

#[cfg(feature = "put")]
macro_rules! mon_length_count (
    ($o:ident, $v:ident, $len:ident!( $($largs:tt)* ), $item:ident!( $($iargs:tt)* )) => (
        {
            let n = &($v.len() as u32);
            let mut res = true;
            res = res && $len!($o, n, $($largs)* );
            for i in 0..*n {
                let v = &$v[i as usize];
                res = res && $item!($o, v, $($iargs)* );
            }
            res
        }
    );
    ($o:ident, $v:ident, $len:expr, $item:expr) => (
        mon_length_count!($o, $v, mon_call!($len), mon_call!($item))
    );
);

#[cfg(feature = "put")]
macro_rules! mon_value (
    ($o:ident, $v:ident, Vec::default()) => (
        $v.is_empty()
    );
    ($o:ident, $v:ident, $val:pat) => (
        // NOTE: will warn if $val is the only variant
        match *$v {
            $val => true,
               _ => false,
        }
    );
    ($o:ident, $v:ident, $val:pat, $junk:expr, $f:ident!( $($args:tt)* )) => (
        match *$v {
            $val => { let src = &$junk; $f!($o, src, $($args)*) },
               _ => false,
        }
    );
);

///////////////////////////////////////////////////////////////////////////////////////////////////

w_named!(flag<bool>, switch!(Eat::byte,
    0 => value!(false) |
    1 => value!(true)
));

#[cfg(feature = "put")]
mon_named!(flag<bool>, mon_switch!(Put::byte,
    0 => mon_value!(false) |
    1 => mon_value!(true)
));

///////////////////////////////////////////////////////////////////////////////////////////////////

w_named!(string<String>, do_parse!(
    n: call!(Eat::long) >>
    st: take!(n) >>
    (String::from_utf8(Vec::from(st)).unwrap_or(String::from("<bad utf8>")))
));

#[cfg(feature = "put")]
impl Put {
    fn string(o: &mut Vec<u8>, v: &String) -> bool {
        let s = v.as_bytes();
        if Put::long(o, &(s.len() as u32)) {
            o.extend_from_slice(s);
            true
        } else {
            false
        }
    }
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

impl H3MSize {
    fn side(&self) -> usize {
        use H3MSize::*;
        match *self {
            S => 36,
            M => 72,
            L => 108,
            XL => 144,
        }
    }
}

impl H3MHeader {
    fn get_width(&self) -> usize {
        self.size.side()
    }
    fn get_height(&self) -> usize {
        self.size.side()
    }
}

w_named!(header<H3MHeader>, do_parse!(
    version: call!(Eat::version) >>
    has_players: call!(Eat::flag) >>
    size: call!(Eat::size) >>
    has_underground: call!(Eat::flag) >>
    name: call!(Eat::string) >>
    description: call!(Eat::string) >>
    difficulty: call!(Eat::difficulty) >>
    level_cap: ifeq!(version, H3MVersion::RoE, value!(0u8), call!(Eat::byte)) >>
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

#[cfg(feature = "put")]
mon_named!(header<H3MHeader>, mon_do_parse!(
    version: mon_call!(Put::version) >>
    has_players: mon_call!(Put::flag) >>
    size: mon_call!(Put::size) >>
    has_underground: mon_call!(Put::flag) >>
    name: mon_call!(Put::string) >>
    description: mon_call!(Put::string) >>
    difficulty: mon_call!(Put::difficulty) >>
    level_cap: mon_ifeq!(version, &H3MVersion::RoE, mon_value!(0u8), mon_call!(Put::byte)) >>
    (H3MHeader {
        ref version,
        ref has_players,
        ref size,
        ref has_underground,
        ref name,
        ref description,
        ref difficulty,
        ref level_cap,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

struct H3MLocation(u8, u8, bool);

w_named!(location<H3MLocation>, do_parse!(
    x: call!(Eat::byte) >> y: call!(Eat::byte) >> u: call!(Eat::flag) >>
    (H3MLocation(x, y, u))
));

#[cfg(feature = "put")]
mon_named!(location<H3MLocation>, mon_do_parse!(
    x: mon_call!(Put::byte) >> y: mon_call!(Put::byte) >> u: mon_call!(Put::flag) >>
    (H3MLocation(ref x, ref y, ref u))
));

impl std::fmt::Debug for H3MLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{} {}{}>", self.0, self.1, if self.2 { " U" } else { "" })
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

struct H3MSpellsMask(u32, u32, u8);

w_named!(spells_mask<H3MSpellsMask>,
       map!(tuple!(Eat::long, Eat::long, Eat::byte), |t| H3MSpellsMask(t.0, t.1, t.2))
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

w_named_args!(main_town(version: H3MVersion)<H3MMainTown>, do_parse!(
    generate_hero: ifeq!(version, H3MVersion::RoE, value!(true), call!(Eat::flag)) >>
    kind: ifeq!(version, H3MVersion::RoE, value!(H3MTownKind::Random), call!(Eat::town_kind)) >>
    location: call!(Eat::location) >>
    (H3MMainTown { generate_hero, kind, location })
));

#[cfg(feature = "put")]
mon_named_args!(main_town(version: H3MVersion)<H3MMainTown>, mon_do_parse!(
    generate_hero: mon_ifeq!(version, H3MVersion::RoE, mon_value!(true), mon_call!(Put::flag)) >>
    kind: mon_ifeq!(version, H3MVersion::RoE, mon_value!(H3MTownKind::Random), mon_call!(Put::town_kind)) >>
    location: mon_call!(Put::location) >>
    (H3MMainTown { ref generate_hero, ref kind, ref location })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

struct H3MResources([u32; 7]);

w_named!(resources<H3MResources>, map!(count_fixed!(u32, Eat::long, 7), |xs| H3MResources(xs)));

impl std::fmt::Debug for H3MResources {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<W:{} M:{} O:{} S:{} C:{} G:{} $:{}>",
               self.0[0], self.0[1], self.0[2], self.0[3], self.0[4], self.0[5], self.0[6]
        )
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MArtifact(u8, u8);

w_named!(artifact1<H3MArtifact>, map!(Eat::byte, |i| H3MArtifact(i, 0)));
w_named!(artifact2<H3MArtifact>, do_parse!(i: call!(Eat::byte) >> j: call!(Eat::byte) >> (H3MArtifact(i, j))));

w_named_args!(artifact(version: H3MVersion)<H3MArtifact>,
    ifeq!(version, H3MVersion::RoE, call!(Eat::artifact1), call!(Eat::artifact2))
);

mon_named!(artifact1<H3MArtifact>, mon_map!(Put::byte, |i| H3MArtifact(ref i, 0)));
mon_named!(artifact2<H3MArtifact>, mon_do_parse!(i: mon_call!(Put::byte) >> j: mon_call!(Put::byte) >> (H3MArtifact(ref i, ref j))));

mon_named_args!(artifact(version: H3MVersion)<H3MArtifact>,
    mon_ifeq!(version, H3MVersion::RoE, mon_call!(Put::artifact1), mon_call!(Put::artifact2))
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy)]
struct H3MCreature(u8, u8);

w_named!(creature1<H3MCreature>, map!(Eat::byte, |i| H3MCreature(i, 0)));
w_named!(creature2<H3MCreature>, do_parse!(i: call!(Eat::byte) >> j: call!(Eat::byte) >> (H3MCreature(i, j))));

w_named_args!(creature(version: H3MVersion)<H3MCreature>,
    ifeq!(version, H3MVersion::RoE, call!(Eat::creature1), call!(Eat::creature2))
);

mon_named!(creature1<H3MCreature>, mon_map!(Put::byte, |i| H3MCreature(ref i, 0)));
mon_named!(creature2<H3MCreature>, mon_do_parse!(i: mon_call!(Put::byte) >> j: mon_call!(Put::byte) >> (H3MCreature(ref i, ref j))));

mon_named_args!(creature(version: H3MVersion)<H3MCreature>,
    mon_ifeq!(version, H3MVersion::RoE, mon_call!(Put::creature1), mon_call!(Put::creature2))
);

///////////////////////////////////////////////////////////////////////////////////////////////////

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

#[derive(Debug)]
struct H3MSpecialVictoryCondition {
    condition: H3MVictoryCondition,
    or_default: bool,
    cpu_allowed: bool,
}

w_named_args!(special_victory(version: H3MVersion)<Option<H3MSpecialVictoryCondition>>,
    alt!(
        tag!([0xFF]) => { |_| None } |
        do_parse!(
            code: call!(Eat::byte) >>
            or_default: call!(Eat::flag) >>
            cpu_allowed: call!(Eat::flag) >>
            condition: switch!(variable!(code),
                0x00 => map!(call!(Eat::artifact, version), |art| H3MVictoryCondition::AcquireArtifact(art)) |
                0x01 => do_parse!(cr: call!(Eat::creature, version) >> amount: call!(Eat::long) >>
                    (H3MVictoryCondition::AccumCreatures(cr, amount))) |
                0x02 => do_parse!(res: call!(Eat::resource) >> amount: call!(Eat::long) >>
                    (H3MVictoryCondition::AccumResources(res, amount))) |
                0x03 => do_parse!(loc: call!(Eat::location) >> hall: call!(Eat::hall_level) >> castle: call!(Eat::castle_level) >>
                    (H3MVictoryCondition::UpgradeTown(loc, hall, castle))) |
                0x04 => map!(alt!(tag!([0xFF; 3]) => { |_| None } | call!(Eat::location) => { |x| Some(x) }),
                    |loc| H3MVictoryCondition::BuildGrail(loc)) |
                0x05 => map!(Eat::location, |loc| H3MVictoryCondition::DefeatHero(loc)) |
                0x06 => map!(Eat::location, |loc| H3MVictoryCondition::CaptureTown(loc)) |
                0x07 => map!(Eat::location, |loc| H3MVictoryCondition::DefeatMonster(loc)) |
                0x08 => value!(H3MVictoryCondition::FlagAllDwellings) |
                0x09 => value!(H3MVictoryCondition::FlagAllMines) |
                0x0A => do_parse!(art: call!(Eat::artifact1) >> loc: call!(Eat::location) >>
                    (H3MVictoryCondition::TransportArtifact(art, loc)))
            ) >>
            _code: forget!(code, 0xFF) >>
            (H3MSpecialVictoryCondition {
                condition, or_default, cpu_allowed
            })
        ) => { |x| Some(x) }
    )
);

#[cfg(feature = "put")]
mon_named_args!(special_victory(version: H3MVersion)<Option<H3MSpecialVictoryCondition>>,
    mon_alt!(
        mon_tag!([0xFF]) => { |_| None } |
        mon_do_parse!(
            code: mon_call!(Put::byte) >>
            or_default: mon_call!(Put::flag) >>
            cpu_allowed: mon_call!(Put::flag) >>
            condition: mon_switch!(mon_variable!(code),
                0x00 => mon_map!(mon_call!(Put::artifact, version), |art| H3MVictoryCondition::AcquireArtifact(ref art)) |
                0x01 => mon_do_parse!(cr: mon_call!(Put::creature, version) >> amount: mon_call!(Put::long) >>
                    (H3MVictoryCondition::AccumCreatures(ref cr, ref amount))) |
                0x02 => mon_do_parse!(res: mon_call!(Put::resource) >> amount: mon_call!(Put::long) >>
                    (H3MVictoryCondition::AccumResources(ref res, ref amount))) |
                0x03 => mon_do_parse!(loc: mon_call!(Put::location) >> hall: mon_call!(Put::hall_level) >> castle: mon_call!(Put::castle_level) >>
                    (H3MVictoryCondition::UpgradeTown(ref loc, ref hall, ref castle))) |
                0x04 => mon_map!(mon_alt!(mon_tag!([0xFF; 3]) => { |_| None } | mon_call!(Put::location) => { |x| Some(ref x) }),
                    |loc| H3MVictoryCondition::BuildGrail(ref loc)) |
                0x05 => mon_map!(Put::location, |loc| H3MVictoryCondition::DefeatHero(ref loc)) |
                0x06 => mon_map!(Put::location, |loc| H3MVictoryCondition::CaptureTown(ref loc)) |
                0x07 => mon_map!(Put::location, |loc| H3MVictoryCondition::DefeatMonster(ref loc)) |
                0x08 => mon_value!(H3MVictoryCondition::FlagAllDwellings) |
                0x09 => mon_value!(H3MVictoryCondition::FlagAllMines) |
                0x0A => mon_do_parse!(art: mon_call!(Put::artifact1) >> loc: mon_call!(Put::location) >>
                    (H3MVictoryCondition::TransportArtifact(ref art, ref loc)))
            ) >>
            _code: mon_forget!(code, 0xFF) >>
            (H3MSpecialVictoryCondition {
                ref condition, ref or_default, ref cpu_allowed
            })
        ) => { |x| Some(ref x) }
    )
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MLossCondition {
    Default,
    LoseTown(H3MLocation),
    LoseHero(H3MLocation),
    TimeExpires(u16),
}

w_named!(loss<H3MLossCondition>, switch!(Eat::byte,
    0xFF => value!(H3MLossCondition::Default) |
    0x00 => do_parse!(l: call!(Eat::location) >> (H3MLossCondition::LoseTown(l))) |
    0x01 => do_parse!(l: call!(Eat::location) >> (H3MLossCondition::LoseHero(l))) |
    0x02 => do_parse!(d: call!(Eat::short) >> (H3MLossCondition::TimeExpires(d)))
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MHero {
    face: u8,
    name: String,
}

w_named!(hero<H3MHero>, do_parse!(
    face: call!(Eat::byte) >>
    name: call!(Eat::string) >>
    (H3MHero { face, name })
));

#[cfg(feature = "put")]
mon_named!(hero<H3MHero>, mon_do_parse!(
    face: mon_call!(Put::byte) >>
    name: mon_call!(Put::string) >>
    (H3MHero { ref face, ref name })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MPlayerPlayability {
    human: bool,
    computer: bool,
    behavior: H3MPlayerBehavior,
}

w_named!(player_playability<H3MPlayerPlayability>, do_parse!(
    human: call!(Eat::flag) >>
    computer: call!(Eat::flag) >>
    behavior: call!(Eat::player_behavior) >>
    (H3MPlayerPlayability { human, computer, behavior })
));

#[cfg(feature = "put")]
mon_named!(player_playability<H3MPlayerPlayability>, mon_do_parse!(
    human: mon_call!(Put::flag) >>
    computer: mon_call!(Put::flag) >>
    behavior: mon_call!(Put::player_behavior) >>
    (H3MPlayerPlayability { ref human, ref computer, ref behavior })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MPlayerAllowedAlignments {
    unknown: bool, // SoD
    mask: u8,
    mask_ext: u8, // AB/SoD
    random: bool,
}

w_named_args!(player_allowed_alignments(version: H3MVersion, playable: bool)<H3MPlayerAllowedAlignments>,
    ifeq!(playable, true,
        do_parse!(
            unknown: sod!(version, value!(false), call!(Eat::flag)) >>
            mask: call!(Eat::byte) >>
            mask_ext: ifeq!(version, H3MVersion::RoE, value!(1u8), call!(Eat::byte)) >>
            random: call!(Eat::flag) >>
            (H3MPlayerAllowedAlignments {
                unknown, mask, mask_ext, random,
            })
        ),
        value!(
            H3MPlayerAllowedAlignments { unknown: false, mask: 0u8, mask_ext: 0u8, random: false },
            take!(match version { H3MVersion::RoE => 2, H3MVersion::AB => 3, H3MVersion::SoD => 4 })
        )
        // NOTE: if player is not playable, this contains junk, just eat it
    )
);

#[cfg(feature = "put")]
mon_named_args!(player_allowed_alignments(version: H3MVersion, playable: bool)<H3MPlayerAllowedAlignments>,
    mon_ifeq!(playable, true,
        mon_do_parse!(
            unknown: mon_sod!(version, mon_value!(false), mon_call!(Put::flag)) >>
            mask: mon_call!(Put::byte) >>
            mask_ext: mon_ifeq!(version, H3MVersion::RoE, mon_value!(1u8), mon_call!(Put::byte)) >>
            random: mon_call!(Put::flag) >>
            (H3MPlayerAllowedAlignments {
                ref unknown, ref mask, ref mask_ext, ref random,
            })
        ),
        mon_value!(
            H3MPlayerAllowedAlignments { unknown: false, mask: 0u8, mask_ext: 0u8, random: false },
            [0xAA, 0xBB, 0xCC, 0xDD], // NOTE: source of junk
            mon_take!(match version { H3MVersion::RoE => 2, H3MVersion::AB => 3, H3MVersion::SoD => 4 })
        )
        // NOTE: if player is not playable, this contains junk, just eat it
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

w_named_args!(player(version: H3MVersion)<H3MPlayer>, do_parse!(
    playability: call!(Eat::player_playability) >>
    allowed_alignments: call!(Eat::player_allowed_alignments, version, playability.human || playability.computer) >>
    main_town: option!(call!(Eat::main_town, version)) >>
    random_hero: call!(Eat::flag) >>
    hero_type: call!(Eat::byte) >>
    main_hero: ifeq!(hero_type, 0xFFu8, value!(None), map!(Eat::hero, |x| Some(x))) >>
    num_placeholders: ifeq!(version, H3MVersion::RoE, value!(0u8), call!(Eat::byte)) >>
    heroes: ifeq!(version, H3MVersion::RoE, value!(Vec::default()), length_count!(Eat::long, Eat::hero)) >>
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

#[cfg(feature = "put")]
mon_named_args!(player(version: H3MVersion)<H3MPlayer>, mon_do_parse!(
    playability: mon_call!(Put::player_playability) >>
    allowed_alignments: mon_call!(Put::player_allowed_alignments, version, playability.human || playability.computer) >>
    main_town: mon_option!(mon_call!(Put::main_town, version)) >>
    random_hero: mon_call!(Put::flag) >>
    hero_type: mon_call!(Put::byte) >>
    main_hero: mon_ifeq!(hero_type, &0xFFu8, mon_value!(None), mon_map!(Put::hero, |x| Some(ref x))) >>
    num_placeholders: mon_ifeq!(version, H3MVersion::RoE, mon_value!(0u8), mon_call!(Put::byte)) >>
    heroes: mon_ifeq!(version, H3MVersion::RoE, mon_value!(Vec::default()), mon_length_count!(Put::long, Put::hero)) >>
    (H3MPlayer {
        ref playability,
        ref allowed_alignments,
        ref main_town,
        ref random_hero,
        ref hero_type,
        ref main_hero,
        ref num_placeholders,
        ref heroes,
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

w_named!(hero_availability<H3MHeroAvailability>, do_parse!(
    id: call!(Eat::byte) >>
    face: call!(Eat::byte) >>
    name: call!(Eat::string) >>
    players_mask: call!(Eat::byte) >>
    (H3MHeroAvailability { id, face, name, players_mask })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MAvailableHeroes {
    mask: [u8; 16],
    mask_ext: [u8; 4], // AB/SoD
    settings: Vec<H3MHeroAvailability>, // SoD
}

w_named_args!(available_heroes(version: H3MVersion)<H3MAvailableHeroes>, do_parse!(
    mask: count_fixed!(u8, Eat::byte, 16) >>
    mask_ext: ifeq!(version, H3MVersion::RoE, value!([0xFF, 0xFF, 1, 0]), count_fixed!(u8, Eat::byte, 4)) >>
    _zeroes: ifeq!(version, H3MVersion::RoE, value!(()), value!((), tag!([0u8; 4]))) >>
    settings: sod!(version, value!(Vec::default()), length_count!(Eat::byte, Eat::hero_availability)) >>
    (H3MAvailableHeroes { mask, mask_ext, settings })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

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

w_named_args!(hero_equipment(version: H3MVersion)<H3MHeroEquipment>, do_parse!(
    head: call!(Eat::artifact, version) >>
    shoulders: call!(Eat::artifact, version) >>
    neck: call!(Eat::artifact, version) >>
    rhand: call!(Eat::artifact, version) >>
    lhand: call!(Eat::artifact, version) >>
    torso: call!(Eat::artifact, version) >>
    rring: call!(Eat::artifact, version) >>
    lring: call!(Eat::artifact, version) >>
    feet: call!(Eat::artifact, version) >>
    misc1: call!(Eat::artifact, version) >>
    misc2: call!(Eat::artifact, version) >>
    misc3: call!(Eat::artifact, version) >>
    misc4: call!(Eat::artifact, version) >>
    machine1: call!(Eat::artifact, version) >>
    machine2: call!(Eat::artifact, version) >>
    machine3: call!(Eat::artifact, version) >>
    machine4: call!(Eat::artifact, version) >>
    spellbook: call!(Eat::artifact, version) >>
    misc5: sod!(version, value!(H3MArtifact(0xFF, 0xFF)), call!(Eat::artifact2)) >>
    backpack: length_count!(Eat::short, call!(Eat::artifact, version)) >>
    (H3MHeroEquipment {
        head, shoulders, neck,
        rhand, lhand, torso, rring, lring, feet,
        misc1, misc2, misc3, misc4, misc5,
        machine1, machine2, machine3, machine4,
        spellbook, backpack,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

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

w_named!(hero_customization<H3MHeroCustomization>, do_parse!(
    exp: option!(Eat::long) >>
    skills: option!(length_count!(Eat::long, tuple!(Eat::skill, Eat::skill_level))) >>
    equipment: option!(call!(Eat::hero_equipment, H3MVersion::SoD)) >>
    bio: option!(Eat::string) >>
    gender: call!(Eat::hero_gender) >>
    spells: option!(Eat::spells_mask) >>
    stats: option!(tuple!(Eat::byte, Eat::byte, Eat::byte, Eat::byte)) >>
    (H3MHeroCustomization {
        exp, skills, equipment, bio, gender, spells, stats,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

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

w_named!(tile<H3MTile>, do_parse!(
    terrain: call!(Eat::terrain_type) >>
    texture: call!(Eat::byte) >>
    river_type: call!(Eat::river_type) >>
    river_topo: call!(Eat::river_topo) >>
    road_type: call!(Eat::road_type) >>
    road_topo: call!(Eat::road_topo) >>
    mirror: call!(Eat::byte) >>
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

///////////////////////////////////////////////////////////////////////////////////////////////////

struct H3MMap {
    tiles: Vec<H3MTile>,
}

impl std::fmt::Debug for H3MMap {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{} tiles>", self.tiles.len())
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MObjectTemplate {
    filename: String,
    shape_mask: [u8; 6],
    visit_mask: [u8; 6],
    terrain_type_mask: u32,
    class: H3MObjectClass,
    subclass: u32,
    group: u8,
    is_overlay: bool,
}

w_named!(object_template<H3MObjectTemplate>, do_parse!(
    filename: call!(Eat::string) >>
    shape_mask: count_fixed!(u8, Eat::byte, 6) >>
    visit_mask: count_fixed!(u8, Eat::byte, 6) >>
    terrain_type_mask: call!(Eat::long) >>
    class: call!(Eat::obj_class) >>
    subclass: call!(Eat::long) >>
    group: call!(Eat::byte) >>
    is_overlay: call!(Eat::flag) >>
    _zeroes: tag!([0u8; 16]) >>
    (H3MObjectTemplate {
        filename, shape_mask, visit_mask, terrain_type_mask,
        class: if let (H3MObjectClass::Mine, 7) = (class, subclass) {
            H3MObjectClass::AbandonedMine
        } else { class },
        subclass, group, is_overlay,
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MBuildings {
    Custom([u8; 12]),
    Fort(bool),
}

w_named!(buildings<H3MBuildings>,
    switch!(Eat::flag,
        true => map!(count_fixed!(u8, Eat::byte, 12), |m| H3MBuildings::Custom(m)) |
        false => map!(Eat::flag, |f| H3MBuildings::Fort(f))
    )
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MTownEvent {
    event: H3MEvent,
    buildings: [u8; 6],
    creatures: [u16; 7],
    unknown: u32,
}

w_named_args!(town_event(version: H3MVersion)<H3MTownEvent>, do_parse!(
    event: call!(Eat::event, version) >>
    buildings: count_fixed!(u8, Eat::byte, 6) >>
    creatures: count_fixed!(u16, Eat::short, 7) >>
    unknown: call!(Eat::long) >>
    (H3MTownEvent { event, buildings, creatures, unknown })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MQuantity {
    Random,
    Custom(u16),
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MDwellingFaction {
    SameAsTown(u32),
    Mask(u16),
}

w_named!(dwelling_faction<H3MDwellingFaction>,
    switch!(peek!(Eat::long),
        0 => do_parse!(_zero: call!(Eat::long) >> m: call!(Eat::short) >> (H3MDwellingFaction::Mask(m))) |
        _ => map!(Eat::long, |t| H3MDwellingFaction::SameAsTown(t))
    )
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MMessageAndGuards {
    message: String,
    guards: Option<H3MCreatures>,
}

w_named_args!(msg_guards(version: H3MVersion)<H3MMessageAndGuards>, do_parse!(
    message: call!(Eat::string) >>
    guards: option!(call!(Eat::creatures, version)) >>
    _zeroes: tag!([0u8; 4]) >>
    (H3MMessageAndGuards { message, guards })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MCreatures([(H3MCreature, u16); 7]);

w_named_args!(creatures(version: H3MVersion)<H3MCreatures>,
    map!(count_fixed!((H3MCreature, u16), tuple!(call!(Eat::creature, version), Eat::short), 7), |cs| H3MCreatures(cs))
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MReward {
    Nothing,
    Exp(u32),
    SpellPoints(u32),
    Morale(H3MModifier), // TODO: check if full range is accepted here
    Luck(H3MModifier),   // ui shows only positive choices
    Resource(H3MResource, u32),
    Stat(H3MStat, u8),
    Skill(H3MSkill, H3MSkillLevel),
    Artifact(H3MArtifact),
    Spell(H3MSpell),
    Creatures(H3MCreature, u16),
}

w_named_args!(reward(version: H3MVersion)<H3MReward>,
    switch!(Eat::byte,
        0 => value!(H3MReward::Nothing) |
        1 => map!(Eat::long, |x| H3MReward::Exp(x)) |
        2 => map!(Eat::long, |x| H3MReward::SpellPoints(x)) |
        3 => map!(Eat::modifier, |m| H3MReward::Morale(m)) |
        4 => map!(Eat::modifier, |m| H3MReward::Luck(m)) |
        5 => do_parse!(r: call!(Eat::resource) >> n: call!(Eat::long) >> (H3MReward::Resource(r, n))) |
        6 => do_parse!(s: call!(Eat::stat) >> n: call!(Eat::byte) >> (H3MReward::Stat(s, n))) |
        7 => do_parse!(s: call!(Eat::skill) >> l: call!(Eat::skill_level) >> (H3MReward::Skill(s, l))) |
        8 => map!(call!(Eat::artifact, version), |a| H3MReward::Artifact(a)) |
        9 => map!(Eat::spell, |s| H3MReward::Spell(s)) |
        10 => do_parse!(c: call!(Eat::creature, version) >> n: call!(Eat::short) >> (H3MReward::Creatures(c, n)))
    )
);

///////////////////////////////////////////////////////////////////////////////////////////////////

// Whole struct is AB/SoD
#[derive(Debug)]
enum H3MQuestObjective {
    Nothing,
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

w_named!(quest_objective<H3MQuestObjective>,
    switch!(Eat::byte,
        0 => value!(H3MQuestObjective::Nothing) |
        1 => map!(Eat::long, |x| H3MQuestObjective::Level(x)) |
        2 => map!(tuple!(Eat::byte, Eat::byte, Eat::byte, Eat::byte), |x| H3MQuestObjective::Stats(x)) |
        3 => map!(Eat::long, |x| H3MQuestObjective::DefeatHero(x)) |
        4 => map!(Eat::long, |x| H3MQuestObjective::DefeatMonster(x)) |
        5 => map!(length_count!(Eat::byte, Eat::artifact2), |x| H3MQuestObjective::Artifacts(x)) |
        6 => map!(length_count!(Eat::byte, tuple!(Eat::creature2, Eat::short)), |x| H3MQuestObjective::Creatures(x)) |
        7 => map!(Eat::resources, |x| H3MQuestObjective::Resources(x)) |
        8 => map!(Eat::byte, |x| H3MQuestObjective::Hero(x)) |
        9 => map!(Eat::color, |x| H3MQuestObjective::Color(x))
    )
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MQuest {
    objective: H3MQuestObjective,
    deadline: u32,
    proposal_message: String,
    progress_message: String,
    completion_message: String,
}

// RoE
w_named!(quest1<H3MQuest>,
    map!(Eat::artifact1,
        |a| H3MQuest {
            objective: H3MQuestObjective::Artifacts(vec![a]),
            deadline: 0xFFFFFFFF,
            proposal_message: String::default(),
            progress_message: String::default(),
            completion_message: String::default(),
        }
    )
);

// AB/SoD
w_named!(quest2<H3MQuest>,
    do_parse!(
        objective: call!(Eat::quest_objective) >>
        deadline: call!(Eat::long) >>
        proposal_message: call!(Eat::string) >>
        progress_message: call!(Eat::string) >>
        completion_message: call!(Eat::string) >>
        (H3MQuest {
            objective, deadline, proposal_message, progress_message, completion_message
        })
    )
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MMsgGuardReward {
    guard: Option<H3MMessageAndGuards>,
    exp: u32,
    spell_points: u32, // give/take
    morale: H3MModifier, // give/take
    luck: H3MModifier, // give/take
    resources: H3MResources, // give/take
    stats: (u8, u8, u8, u8),
    skills: Vec<(H3MSkill, H3MSkillLevel)>,
    artifacts: Vec<H3MArtifact>,
    spells: Vec<H3MSpell>,
    creatures: Vec<(H3MCreature, u16)>,
}

w_named_args!(msg_guard_reward(version: H3MVersion)<H3MMsgGuardReward>,
    do_parse!(
        guard: option!(call!(Eat::msg_guards, version)) >>
        exp: call!(Eat::long) >>
        spell_points: call!(Eat::long) >>
        morale: call!(Eat::modifier) >>
        luck: call!(Eat::modifier) >>
        resources: call!(Eat::resources) >>
        stats: tuple!(Eat::byte, Eat::byte, Eat::byte, Eat::byte) >>
        skills: length_count!(Eat::byte, tuple!(Eat::skill, Eat::skill_level)) >>
        artifacts: length_count!(Eat::byte, call!(Eat::artifact, version)) >>
        spells: length_count!(Eat::byte, Eat::spell) >>
        creatures: length_count!(Eat::byte, tuple!(call!(Eat::creature, version), Eat::short)) >>
        _zeroes: tag!([0u8; 8]) >>
        (H3MMsgGuardReward {
            guard, exp, spell_points, morale, luck,
            resources, stats, skills, artifacts, spells, creatures,
        })
    )
);

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum H3MObjectProperties {
    Hero {
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
        spells: Option<H3MSpellsMask>, // AB'/SoD // TODO: set that one spell
        stats: Option<(u8, u8, u8, u8)>, // SoD
    },
    Monster {
        id: u32, // AB/SoD
        quantity: H3MQuantity,
        disposition: H3MDisposition,
        treasure: Option<(String, H3MResources, H3MArtifact)>,
        never_flees: bool,
        no_grow: bool,
    },
    Town {
        id: u32, // AB/SoD
        owner: H3MColor,
        name: Option<String>,
        garrison: Option<H3MCreatures>,
        group_formation: bool,
        buildings: H3MBuildings,
        forced_spells: H3MSpellsMask, // AB/SoD
        allowed_spells: H3MSpellsMask,
        events: Vec<H3MTownEvent>,
        alignment: u8, // SoD
    },
    HeroPlaceholder { owner: H3MColor, id: u8, power_rating: Option<u8> },
    OwnedObject { owner: H3MColor },
    RandomDwelling { owner: H3MColor, faction: H3MDwellingFaction, level_range: (u8, u8) },
    RandomDwellingLevel { owner: H3MColor, faction: H3MDwellingFaction },
    RandomDwellingFaction { owner: H3MColor, level_range: (u8, u8) },
    Resource { guard: Option<H3MMessageAndGuards>, amount: u32 }, // TODO: amount `0`, amount could be random. if gold then 100x
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

w_named_args!(obj_hero(version: H3MVersion)<H3MObjectProperties>, do_parse!(
    id: ifeq!(version, H3MVersion::RoE, value!(0xFFFFFFFF), call!(Eat::long)) >>
    owner: call!(Eat::color) >>
    hero_type: call!(Eat::byte) >>
    name: option!(Eat::string) >>
    exp: sod!(version, map!(Eat::long, |x| Some(x)), option!(Eat::long)) >>
    face: option!(Eat::byte) >>
    skills: option!(length_count!(Eat::long, tuple!(Eat::skill, Eat::skill_level))) >>
    garrison: option!(call!(Eat::creatures, version)) >>
    group_formation: call!(Eat::flag) >>
    equipment: option!(call!(Eat::hero_equipment, version)) >>
    patrol_radius: call!(Eat::byte) >>
    bio: ifeq!(version, H3MVersion::RoE, value!(None), option!(Eat::string)) >>
    gender: ifeq!(version, H3MVersion::RoE, value!(H3MHeroGender::Default), call!(Eat::hero_gender)) >>
    spells: switch!(value!(version),
                    H3MVersion::RoE => value!(None) |
                    H3MVersion::AB => value!(None, Eat::byte) |
                    H3MVersion::SoD => option!(Eat::spells_mask)
    ) >>
    stats: sod!(version, value!(None), option!(tuple!(Eat::byte, Eat::byte, Eat::byte, Eat::byte))) >>
    _zeros: tag!([0u8; 16]) >>
    (H3MObjectProperties::Hero {
        id, owner, hero_type, name, exp, face, skills, garrison, group_formation,
        equipment, patrol_radius, bio, gender, spells, stats,
    })
));

w_named_args!(obj_monster(version: H3MVersion)<H3MObjectProperties>, do_parse!(
    id: ifeq!(version, H3MVersion::RoE, value!(0xFFFFFFFF), call!(Eat::long)) >>
    quantity: switch!(peek!(Eat::short),
        0 => value!(H3MQuantity::Random, Eat::short) |
        _ => map!(Eat::short, |q| H3MQuantity::Custom(q))
    )>>
    disposition: call!(Eat::disposition) >>
    treasure: option!(tuple!(Eat::string, Eat::resources, call!(Eat::artifact, version))) >>
    never_flees: call!(Eat::flag) >>
    no_grow: call!(Eat::flag) >>
    _zeroes: tag!([0u8; 2]) >>
    (H3MObjectProperties::Monster {
        id, quantity, disposition, treasure, never_flees, no_grow,
    })
));

w_named_args!(obj_town(version: H3MVersion)<H3MObjectProperties>, do_parse!(
    id: ifeq!(version, H3MVersion::RoE, value!(0xFFFFFFFF), call!(Eat::long)) >>
    owner: call!(Eat::color) >>
    name: option!(Eat::string) >>
    garrison: option!(call!(Eat::creatures, version)) >>
    group_formation: call!(Eat::flag) >>
    buildings: call!(Eat::buildings) >>
    forced_spells: ifeq!(version, H3MVersion::RoE, value!(H3MSpellsMask::default()), call!(Eat::spells_mask)) >>
    allowed_spells: call!(Eat::spells_mask) >>
    events: length_count!(Eat::long, call!(Eat::town_event, version)) >>
    alignment: sod!(version, value!(0xFF), call!(Eat::byte)) >>
    _zeroes: tag!([0u8; 3]) >>
    (H3MObjectProperties::Town {
        id, owner, name, garrison, group_formation, buildings,
        forced_spells, allowed_spells, events, alignment
    })
));

w_named_args!(obj_placeholder(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: call!(Eat::color) >>
    id: call!(Eat::byte) >>
    power_rating: switch!(value!(id == 0xFF),
                          true => map!(Eat::byte, |x| Some(x)) |
                          false => value!(None)
    ) >>
    (H3MObjectProperties::HeroPlaceholder {
        owner, id, power_rating
    })
));

w_named_args!(obj_owned(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: call!(Eat::color) >>
    _zeroes: tag!([0u8; 3]) >>
    (H3MObjectProperties::OwnedObject { owner })
));

w_named_args!(obj_dwelling(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: call!(Eat::color) >>
    _zeroes: tag!([0u8; 3]) >>
    faction: call!(Eat::dwelling_faction) >>
    level_range: tuple!(Eat::byte, Eat::byte) >>
    (H3MObjectProperties::RandomDwelling { owner, faction, level_range })
));

w_named_args!(obj_dwelling_level(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: call!(Eat::color) >>
    _zeroes: tag!([0u8; 3]) >>
    faction: call!(Eat::dwelling_faction) >>
    (H3MObjectProperties::RandomDwellingLevel { owner, faction  })
));

w_named_args!(obj_dwelling_faction(_v: H3MVersion)<H3MObjectProperties>, do_parse!(
    owner: call!(Eat::color) >>
    _zeroes: tag!([0u8; 3]) >>
    level_range: tuple!(Eat::byte, Eat::byte) >>
    (H3MObjectProperties::RandomDwellingFaction { owner, level_range })
));

w_named_args!(obj_resource(version: H3MVersion)<H3MObjectProperties>, do_parse!(
    guard: option!(call!(Eat::msg_guards, version)) >>
    amount: call!(Eat::long) >>
    _zeroes: tag!([0u8; 4]) >>
    (H3MObjectProperties::Resource { guard, amount })
));

w_named_args!(obj_artifact(version: H3MVersion)<H3MObjectProperties>,
    map!(option!(call!(Eat::msg_guards, version)), |guard| H3MObjectProperties::Artifact { guard })
);

w_named_args!(obj_scroll(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        guard: option!(call!(Eat::msg_guards, version)) >>
        spell: call!(Eat::spell) >>
        _zeros: tag!([0u8; 3]) >>
        (H3MObjectProperties::Scroll { guard, spell })
    )
);

w_named_args!(obj_witch(version: H3MVersion)<H3MObjectProperties>, map!(
    ifeq!(version, H3MVersion::RoE, value!(0x0FFFEFBF), call!(Eat::long)),
    |skills| H3MObjectProperties::Witch { skills }
));

w_named_args!(obj_shrine(_v: H3MVersion)<H3MObjectProperties>,
    do_parse!(spell: call!(Eat::byte) >> _zeroes: tag!([0u8; 3]) >> (H3MObjectProperties::Shrine { spell }))
);

w_named_args!(obj_grail(_v: H3MVersion)<H3MObjectProperties>,
    map!(Eat::long, |radius| H3MObjectProperties::Grail { radius })
);

w_named_args!(obj_message(_v: H3MVersion)<H3MObjectProperties>,
    do_parse!(text: call!(Eat::string) >> _zeroes: tag!([0u8; 4]) >> (H3MObjectProperties::Message { text }))
);

w_named_args!(obj_scholar(_v: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        bonus_type: call!(Eat::byte) >>
        bonus_id: call!(Eat::byte) >>
        _zeroes: tag!([0u8; 6]) >>
        (H3MObjectProperties::Scholar { bonus_type, bonus_id })
    )
);

w_named_args!(obj_abandoned(_v: H3MVersion)<H3MObjectProperties>,
    do_parse!(resources: call!(Eat::byte) >> _zeroes: tag!([0u8; 3]) >> (H3MObjectProperties::AbandonedMine { resources }))
);

w_named_args!(obj_garrison(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        owner: call!(Eat::color) >>
        _zeroes1: tag!([0u8; 3]) >>
        creatures: call!(Eat::creatures, version) >>
        removable: ifeq!(version, H3MVersion::RoE, value!(1u8), call!(Eat::byte)) >> // TODO: meaning, bool?
        _zeroes2: tag!([0u8; 8]) >>
        (H3MObjectProperties::Garrison { owner, creatures, removable })
    )
);

w_named_args!(obj_seer(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        quest: ifeq!(version, H3MVersion::RoE, call!(Eat::quest1), call!(Eat::quest2)) >>
        reward: call!(Eat::reward, version) >>
        _zeroes: tag!([0u8; 2]) >>
        (H3MObjectProperties::Seer { quest, reward })
    )
);

w_named_args!(obj_quest_guard(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        quest: ifeq!(version, H3MVersion::RoE, call!(Eat::quest1), call!(Eat::quest2)) >>
        (H3MObjectProperties::QuestGuard { quest })
    )
);

w_named_args!(obj_pandora(version: H3MVersion)<H3MObjectProperties>, map!(
    call!(Eat::msg_guard_reward, version),
    |contents| H3MObjectProperties::Pandora { contents }
));

w_named_args!(obj_event(version: H3MVersion)<H3MObjectProperties>,
    do_parse!(
        contents: call!(Eat::msg_guard_reward, version) >>
        players_mask: call!(Eat::byte) >>
        ai_allowed: call!(Eat::flag) >>
        one_time: call!(Eat::flag) >>
        _zeroes: tag!([0u8; 4]) >>
        (H3MObjectProperties::Event {
            contents, players_mask, ai_allowed, one_time
        })
    )
);

w_named_args!(obj_noprops(_v: H3MVersion)<H3MObjectProperties>, value!(H3MObjectProperties::NoProperties));

///////////////////////////////////////////////////////////////////////////////////////////////////

impl H3MObjectClass {
    fn props_parser(&self) -> fn (&[u8], H3MVersion) -> nom::IResult<&[u8], H3MObjectProperties> {
        use H3MObjectClass::*;
        match *self {
            Hero | Prison | RandomHero => Eat::obj_hero,
            RandomTown | Town => Eat::obj_town,
            Monster | RandomMonster | RandomMonster1 | RandomMonster2 | RandomMonster3 |
            RandomMonster4 | RandomMonster5 | RandomMonster6 | RandomMonster7 => Eat::obj_monster,
            CreatureGenerator1 | CreatureGenerator2 | CreatureGenerator3 | CreatureGenerator4 |
            Lighthouse | Mine | Shipyard => Eat::obj_owned,
            Artifact | RandomArtifact | RandomTreasureArtifact |
            RandomMinorArtifact | RandomMajorArtifact | RandomRelicArtifact => Eat::obj_artifact,
            PandorasBox => Eat::obj_pandora,
            Event => Eat::obj_event,
            Garrison | Garrison2 => Eat::obj_garrison,
            Grail => Eat::obj_grail,
            OceanBottle | Sign => Eat::obj_message,
            RandomResource | Resource => Eat::obj_resource,
            Scholar => Eat::obj_scholar,
            SeerHut => Eat::obj_seer,
            ShrineOfMagicIncantation | ShrineOfMagicGesture | ShrineOfMagicThought => Eat::obj_shrine,
            SpellScroll => Eat::obj_scroll,
            WitchHut => Eat::obj_witch,
            HeroPlaceholder => Eat::obj_placeholder,
            QuestGuard => Eat::obj_quest_guard,
            RandomDwelling => Eat::obj_dwelling,
            RandomDwellingLevel => Eat::obj_dwelling_level,
            RandomDwellingFaction => Eat::obj_dwelling_faction,
            AbandonedMine => Eat::obj_abandoned,
            _ => Eat::obj_noprops,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct H3MObject {
    loc: H3MLocation,
    template_idx: u32,
    properties: H3MObjectProperties,
}

w_named_args!(object<'a>(version: H3MVersion, templates: &'a[H3MObjectTemplate])<H3MObject>, do_parse!(
    loc: call!(Eat::location) >>
    template_idx: call!(Eat::long) >>
    _zeroes: tag!([0u8; 5]) >>
    properties: call!(templates[template_idx as usize].class.props_parser(), version) >>
    (H3MObject {
        loc, template_idx, properties
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

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

w_named_args!(event(version: H3MVersion)<H3MEvent>, do_parse!(
    name: call!(Eat::string) >>
    text: call!(Eat::string) >>
    resources: call!(Eat::resources) >>
    unknown1: call!(Eat::byte) >>
    unknown2: sod!(version, value!(true), call!(Eat::flag)) >>
    unknown3: call!(Eat::flag) >>
    first_occurence: call!(Eat::short) >>
    repeat_period: call!(Eat::short) >>
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

w_named!(h3m<H3MFile>, do_parse!(
    header: call!(Eat::header) >>
    players: count!(call!(Eat::player, header.version), 8) >>
    victory: call!(Eat::special_victory, header.version) >>
    loss: call!(Eat::loss) >>
    teams: switch!(Eat::byte,
        0u8 => value!(None) |
        _ => map!(count_fixed!(u8, Eat::byte, 8), |x| Some(x))
    ) >>
    available_heroes: call!(Eat::available_heroes, header.version) >>
    _zeroes: tag!([0u8; 31]) >>
    banned_artifacts: ifeq!(header.version, H3MVersion::RoE, value!([0u8; 17]), count_fixed!(u8, Eat::byte, 17)) >>
    banned_artifacts_ext: sod!(header.version, value!(31u8), call!(Eat::byte)) >>
    banned_spells: sod!(header.version, value!(H3MSpellsMask::default()), call!(Eat::spells_mask)) >>
    banned_skills: sod!(header.version, value!(0u32), call!(Eat::long)) >>
    rumors: length_count!(Eat::long, tuple!(Eat::string, Eat::string)) >>
    heroes: count!(sod!(header.version, value!(None), option!(Eat::hero_customization)), 156) >>
    land: count!(Eat::tile, header.get_width() * header.get_height()) >>
    underground: switch!(value!(header.has_underground),
        false => value!(None) |
        true => map!(count!(Eat::tile, header.get_width() * header.get_height()), |tiles| Some(H3MMap { tiles }))
    ) >>
    object_templates: length_count!(Eat::long, Eat::object_template) >>
    objects: length_count!(Eat::long, call!(Eat::object, header.version, &object_templates)) >>
    events: length_count!(Eat::long, call!(Eat::event, header.version)) >>
    _trailing_zeroes: count!(tag!([0u8]), 124) >>
    (H3MFile {
        header, players, victory, loss, teams, available_heroes,
        banned_artifacts, banned_artifacts_ext, banned_spells, banned_skills, rumors, heroes,
        land: H3MMap { tiles: land }, underground,
        object_templates, objects, events
    })
));

#[cfg(feature = "put")]
mon_named!(h3m<H3MFile>, mon_do_parse!(
    header: mon_call!(Put::header) >>
    players: mon_count!(mon_call!(Put::player, header.version), 8) >>
    victory: mon_call!(Put::special_victory, header.version) >>
    // loss: call!(Eat::loss) >>
    // teams: switch!(Eat::byte,
    //     0u8 => value!(None) |
    //     _ => map!(count_fixed!(u8, Eat::byte, 8), |x| Some(x))
    // ) >>
    // available_heroes: call!(Eat::available_heroes, header.version) >>
    // _zeroes: tag!([0u8; 31]) >>
    // banned_artifacts: ifeq!(header.version, H3MVersion::RoE, value!([0u8; 17]), count_fixed!(u8, Eat::byte, 17)) >>
    // banned_artifacts_ext: sod!(header.version, value!(31u8), call!(Eat::byte)) >>
    // banned_spells: sod!(header.version, value!(H3MSpellsMask::default()), call!(Eat::spells_mask)) >>
    // banned_skills: sod!(header.version, value!(0u32), call!(Eat::long)) >>
    // rumors: length_count!(Eat::long, tuple!(Eat::string, Eat::string)) >>
    // heroes: count!(sod!(header.version, value!(None), option!(Eat::hero_customization)), 156) >>
    // land: count!(Eat::tile, header.get_width() * header.get_height()) >>
    // underground: switch!(value!(header.has_underground),
    //     false => value!(None) |
    //     true => map!(count!(Eat::tile, header.get_width() * header.get_height()), |tiles| Some(H3MMap { tiles }))
    // ) >>
    // object_templates: length_count!(Eat::long, Eat::object_template) >>
    // objects: length_count!(Eat::long, call!(Eat::object, header.version, &object_templates)) >>
    // events: length_count!(Eat::long, call!(Eat::event, header.version)) >>
    // _trailing_zeroes: count!(tag!([0u8]), 124) >>
    (H3MFile {
        ref header, ref players, ref victory, .. /* loss, teams, available_heroes,
        banned_artifacts, banned_artifacts_ext, banned_spells, banned_skills, rumors, heroes,
        land: H3MMap { tiles: land }, underground,
        object_templates, objects, events */
    })
));

///////////////////////////////////////////////////////////////////////////////////////////////////

impl H3MRoadTopology {
    fn to_ascii(&self) -> &[u8] {
        use H3MRoadTopology::*;
        match *self {
            Turn1   => b".   oo o ",
            Turn2   => b".   ** * ",
            Diag1   => b".. . o o ",
            Diag2   => b".. . * * ",
            Diag3   => b".. . o * ",
            Diag4   => b".. . * o ",
            TVert1  => b" o  oo o ",
            TVert2  => b" *  ** * ",
            THorz1  => b"   ooo o ",
            THorz2  => b"   *** * ",
            Vert1   => b" o  o  o ",
            Vert2   => b" *  *  * ",
            Horz1   => b"   ooo   ",
            Horz2   => b"   ***   ",
            EndVert => b". . o  o ",
            EndHorz => b".   oo.  ",
            Cross   => b" o ooo o ",
        }
    }
}

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
                    let pat = tile.road_topo.to_ascii();
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
                use H3MTerrainType::*;
                use colored::Color;
                let color = match tile.terrain {
                    TrDirt => Color::Yellow,
                    TrSand => Color::BrightYellow,
                    TrGrass => Color::Green,
                    TrSnow => Color::BrightWhite,
                    TrSwamp => Color::Cyan,
                    TrRough => Color::Magenta,
                    TrSubterranean => Color::BrightBlack,
                    TrLava => Color::Red,
                    TrWater => Color::Blue,
                    TrRock => Color::Black,
                };
                print!("{}", line.color(color));
            }
            println!();
        }
    }
}

fn hex_dump(bytes: &[u8]) -> String {
    use std::fmt::Write;
    let mut dump = String::new();
    let mut count: usize = 0;
    for byte in bytes {
        write!(dump, "{:02X} ", byte).unwrap();
        count += 1;
        if count % 32 == 0 {
            dump.push_str("\n");
        }
    }
    dump
}

fn main() {
    let matches = clap::App::new("h3m")
        .arg(clap::Arg::with_name("binflag").short("b").long("bin"))
        .arg(clap::Arg::with_name("drawflag").short("m").long("map"))
        .arg(clap::Arg::with_name("printflag").short("p").long("print"))
        .arg(clap::Arg::with_name("INPUT").required(true).index(1))
        .get_matches();

    let read_file = |f| {
        let br = BufReader::new(f);
        let mut buf: Vec<u8> = Vec::new();
        GzDecoder::new(br).read_to_end(&mut buf).map(move |_| buf)
    };

    let res = matches.value_of("INPUT").ok_or("no input".to_owned()).and_then(
        |p| File::open(p)
            .and_then(read_file)
            .map_err(|e| e.to_string())
    );

    match res {
        Ok(bin) => {
            if matches.is_present("binflag") {
                println!("unzipped size: {}\n{}", bin.len(), hex_dump(&bin));
            }

            match Eat::h3m(&bin) {
                Ok((rem, doc)) => {
                    if matches.is_present("printflag") {
                        println!("parsed document: {:#?}", doc);
                    }

                    if matches.is_present("drawflag") {
                        print_map(&doc);
                    }

                    if rem.len() > 0 {
                        panic!("remaining: {:?}", rem.len());
                    }

                    #[cfg(feature = "put")]
                    {
                        let out = &mut Vec::new();
                        assert_eq!(Put::h3m(out, &doc), true);

                        let ori = hex_dump(&bin[0..out.len()]);
                        let rev = hex_dump(&out);

                        println!("original_dump:\n{}\n", &ori);
                        println!("reversed_dump:\n{}\n", &rev);
                        println!("{}\n", if ori == rev {"same"} else {"different"});
                    }
                }
                Err(e) => panic!("error: {:#?}", e),
            }
        },
        Err(err) => panic!("error: {}", err),
    }
}
