use super::SDF;

use bevy::math::*;

fn ndot(a: Vec2, b: Vec2) -> f32 {
    return a.x * b.x - a.y * b.y;
}

macro_rules! sdf_impl_with_vec{
    (
        $(#[$outer:meta])*
        $vis:vis struct $Struct:ident<V>:$UStruct:ident<V> {
            $(
                $(#[$geninner:ident $($Genargs:tt)*])*
                V $gen_inner_vis:vis $GenMember:ident,
            )+
            $(
                $(#[$inner:ident $($args:tt)*])*
                $inner_vis:vis $Member:ident: $T:ty,
            )*
        }

        fn dist($Self:ident, $Point:ident: V) -> Self::Item $Impl:expr
    ) => {
        $(#[$outer])*
        #[derive(Copy, Clone)]
        $vis struct $Struct<V> {
        $(
            $(#[$geninner $($Genargs)*])*
            $gen_inner_vis $GenMember: V,
        )+
        $(
            $(#[$inner $($args)*])*
            $inner_vis $Member: $T,
        )*
        }

        $(#[$outer])*
        #[derive(Copy, Clone)]
        $vis struct $UStruct<U, V> {
        $(
            $(#[$geninner $($Genargs)*])*
            $gen_inner_vis $GenMember: V,
        )+
        $(
            $(#[$inner $($args)*])*
            $inner_vis $Member: $T,
        )*
            pub data: U,
        }

        impl<V> $Struct<V> {
            pub fn new(
                $(
                    $GenMember: V,
                )+
                $(
                    $Member : $T,
                )*
            ) -> Self {
                Self {
                    $(
                        $GenMember,
                    )*
                    $(
                        $Member,
                    )*
                }
            }
        }

        impl SDF<Vec3A, ()> for &$Struct<Vec3A> {
            type Item = f32;
            fn dist($Self, $Point: Vec3A) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3A;
                $Impl
            }
        }

        impl SDF<Vec3, ()> for &$Struct<Vec3> {
            type Item = f32;
            fn dist($Self, $Point: Vec3) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3;
                $Impl
            }
        }

        impl<U, V> $UStruct<U, V> {
            pub fn new(
                $(
                    $GenMember: V,
                )+
                $(
                    $Member : $T,
                )*
                data: U,
            ) -> Self {
                Self {
                    $(
                        $GenMember,
                    )*
                    $(
                        $Member,
                    )*
                    data,
                }
            }
        }

        impl<'a, U> SDF<Vec3, U> for &'a $UStruct<U, Vec3> {
            type Item = (f32, &'a U);
            fn dist($Self, $Point: Vec3) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3;
                ($Impl, &$Self.data)
            }
        }

        impl<'a, U> SDF<Vec3A, U> for &'a $UStruct<U, Vec3A> {
            type Item = (f32, &'a U);
            fn dist($Self, $Point: Vec3A) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3A;
                ($Impl, &$Self.data)
            }
        }
    }
}

macro_rules! sdf_impl{
    (
        $(#[$outer:meta])*
        $vis:vis struct $Struct:ident:$UStruct:ident {
            $(
                $(#[$inner:ident $($args:tt)*])*
                $inner_vis:vis $Member:ident: $T:ty,
            )*
        }

        fn dist($Self:ident, $Point:ident: V) -> Self::Item $Impl:expr
    ) => {
        $(#[$outer])*
        #[derive(Copy, Clone)]
        $vis struct $Struct {
        $(
            $(#[$inner $($args)*])*
            $inner_vis $Member: $T,
        )*
        }

        $(#[$outer])*
        #[derive(Copy, Clone)]
        $vis struct $UStruct<U> {
        $(
            $(#[$inner $($args)*])*
            $inner_vis $Member: $T,
        )*
            pub data: U,
        }

        impl $Struct {
            pub fn new(
                $(
                    $Member : $T,
                )*
            ) -> Self {
                Self {
                    $(
                        $Member,
                    )*
                }
            }
        }

        impl SDF<Vec3A, ()> for &$Struct {
            type Item = f32;
            fn dist($Self, $Point: Vec3A) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3A;
                $Impl
            }
        }

        impl SDF<Vec3, ()> for &$Struct {
            type Item = f32;
            fn dist($Self, $Point: Vec3) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3;
                $Impl
            }
        }

        impl<U> $UStruct<U> {
            pub fn new(
                $(
                    $Member : $T,
                )*
                data: U,
            ) -> Self {
                Self {
                    $(
                        $Member,
                    )*
                    data,
                }
            }
        }

        impl<'a, U> SDF<Vec3A, U> for &'a $UStruct<U> {
            type Item = (f32, &'a U);
            fn dist($Self, $Point: Vec3A) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3A;
                ($Impl, &$Self.data)
            }
        }

        impl<'a, U> SDF<Vec3, U> for &'a $UStruct<U> {
            type Item = (f32, &'a U);
            fn dist($Self, $Point: Vec3) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3;
                ($Impl, &$Self.data)
            }
        }
    }
}

sdf_impl!(
    pub struct Sphere:USphere {
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        point.length() - self.radius
    }
);

sdf_impl_with_vec!(
    pub struct Box<V>:UBox<V> {
        V pub dims,
    }

    fn dist(self, point: V) -> Self::Item {
        let q = self.dims.abs() - point;
        (q.max(V::ZERO) + q.max_element().min(0.0)).length()
    }
);

sdf_impl_with_vec!(
    pub struct RoundBox<V>:URoundBox<V> {
        V pub dims,
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let q = self.dims.abs() - point;
        (q.max(V::ZERO) + q.max_element().min(0.0)).length() - self.radius
    }
);

sdf_impl_with_vec!(
    pub struct BoxFrame<V>:UBoxFrame<V> {
        V pub dims,
        pub thickness: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let dims = self.dims.abs() - point;
        let q = (dims+self.thickness).abs() - self.thickness;
        (Into::<V>::into(vec3(dims.x, q.y, q.z)).max(V::ZERO)+q.y.max(q.z).max(dims.x).min(0.0)).length().min(
        (Into::<V>::into(vec3(q.x, dims.y, q.z)).max(V::ZERO)+dims.y.max(q.z).max(q.x).min(0.0)).length()).min(
        (Into::<V>::into(vec3(q.x, q.y, dims.z)).max(V::ZERO)+q.y.max(dims.z).max(q.x).min(0.0)).length())
    }
);

sdf_impl!(
    pub struct Torus:UTorus {
        pub thickness: f32,
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let q = vec2(point.xz().length() - self.thickness, point.y);
        q.length() - self.radius
    }
);

sdf_impl!(
    pub struct CappedTorus:UCappedTorus {
        pub thickness: f32,
        pub radius: f32,
        pub completeness: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let mut point = point;
        point.x = point.x.abs();
        let sc = vec2(self.completeness.sin(),self.completeness.cos());
        let k = if sc.y*point.x>sc.x*point.y { sc.dot(point.xy()) } else { point.xy().length() };
        ((point.dot(point) + self.radius*self.radius - 2.0*self.radius*k) - self.radius).sqrt()
    }
);

sdf_impl!(
    pub struct Link:ULink {
        pub length: f32,
        pub thickness: f32,
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let q = vec3(point.x, (point.y.abs() - self.length).max(0.0), point.z);
        vec2(q.xy().length() - self.thickness,q.z).length() - self.radius
    }
);

sdf_impl_with_vec!(
    pub struct ICylinder<V>:UICylinder<V> {
        V pub dims,
    }

    fn dist(self, point: V) -> Self::Item {
        (point.xz() - self.dims.xy()).length() - self.dims.z
    }
);

sdf_impl!(
    pub struct Cone:UCone {
        pub height: f32,
        pub angle: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let c = vec2(self.angle.sin(), self.angle.cos());
        let q = point.xz().length();
        c.dot(vec2(q, point.y)).max(-self.height-point.y)
    }
);

sdf_impl_with_vec!(
    pub struct Plane<V>:UPlane<V> {
        V pub normal,
        pub height: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        point.dot(self.normal) + self.height
    }
);

sdf_impl!(
    pub struct HexPrism:UHexPrism {
        pub height: f32,
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let k = Into::<V>::into(vec3(-0.8660254, 0.5, 0.57735));
        let point = point.abs();
        let point = Into::<V>::into((point.xy() - 2.0*k.xy().dot(point.xy()).min(0.0), point.z));
        let d = vec2(
            (point.xy()-vec2(point.x.clamp(-k.z*self.radius, k.z*self.radius), self.radius)).length()*(point.y-self.radius).signum(),
            point.z-self.height
        );
        d.x.max(d.y).min(0.0) + d.max(Vec2::ZERO).length()
    }
);

sdf_impl!(
    pub struct TriPrism:UTriPrism {
        pub height: f32,
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let q = point.abs();
        (q.z-self.height).max((q.x*0.866025+point.y*0.5).max(-point.y)-self.radius*0.5)
    }
);

sdf_impl!(
    pub struct Capsule:UCapsule {
        pub height: f32,
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let point = Into::<V>::into((point.x, point.y - point.y.clamp(0.0, self.height), point.z));
        point.length() - self.radius
    }
);

sdf_impl!(
    pub struct Cylinder:UCylinder {
        pub height: f32,
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let d = vec2(point.xz().length(), point.y).abs() - vec2(self.radius, self.height);
        d.x.max(d.y).min(0.0) + d.max(Vec2::ZERO).length()
    }
);

sdf_impl!(
    pub struct RoundedCylinder:URoundedCylinder {
        pub height: f32,
        pub cylinder_radius: f32,
        pub round_radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let d = vec2(point.xz().length()-2.0*self.cylinder_radius+self.round_radius, point.y.abs() - self.height);
        d.x.max(d.y).min(0.0) + d.max(Vec2::ZERO).length() - self.round_radius
    }
);

sdf_impl!(
    pub struct CappedCone:UCappedCone {
        pub height: f32,
        pub base_radius: f32,
        pub tip_radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let q = vec2(point.xz().length(), point.y);
        let k1 = vec2(self.tip_radius, self.height);
        let k2 = vec2(self.tip_radius - self.base_radius,2.0*self.height);
        let ca = vec2(q.x-(q.x.min( if q.y<0.0 {self.base_radius} else {self.tip_radius})), q.y.abs() - self.height);
        let cb = q - k1 + k2*((k1-q).dot(k2)/k2.dot(k2)).clamp(0.0, 1.0);
        let s = if cb.x<0.0 &&  ca.y<0.0 {-1.0} else {1.0};
        s* (ca.dot(ca).min(cb.dot(cb))).sqrt()
    }
);

sdf_impl!(
    pub struct SolidAngle:USolidAngle {
        pub angle: f32,
        pub radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let c = vec2(self.angle.sin(), self.angle.cos());
        let q = vec2(point.xz().length(), point.y);
        let l = q.length() - self.radius;
        let m = (q - c*q.dot(c).clamp(0.0, self.radius)).length();
        l.max(m*(c.y * q.x * c.x * q.y).signum())
    }
);

sdf_impl!(
    pub struct CutSphere:UCutSphere {
        pub radius: f32,
        pub height: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let w = (self.radius*(self.radius-self.height)*self.height).sqrt();

        let q = vec2(point.xz().length(), point.y);
        let s = (self.height - self.radius)*q.x*q.x+w*w*(self.height+self.radius-2.0*q.y).max(self.height*q.x-w*q.y);
        if s < 0.0 {q.length()-self.radius} else {if q.x<w {self.height - q.y} else {(q-vec2(w,self.height)).length()}}
    }
);

sdf_impl!(
    pub struct CutHollowSphere:UCutHollowSphere {
        pub radius: f32,
        pub height: f32,
        pub thickness: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let w = (self.radius*self.radius-self.height*self.height).sqrt();

        let q = vec2(point.xz().length(), point.y);
        if self.height*q.x<w*q.y {(q-vec2(w,self.height)).length()} else {(q.length()-self.radius).abs() - self.thickness}
    }
);

sdf_impl!(
    pub struct RoundCone:URoundCone {
        pub height: f32,
        pub base_radius: f32,
        pub tip_radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let b = (self.base_radius - self.tip_radius)/self.height;
        let a = (1.0-b*b).sqrt();

        let q = vec2(point.xz().length(), point.y);
        let k = q.dot(vec2(-b, a));
        if k < 0.0 { q.length() -self.base_radius }
        else if k > a*self.height {(q-vec2(0.0,self.height)).length() - self.tip_radius}
        else { q.dot(vec2(a,b)) - self.base_radius }
    }
);

sdf_impl_with_vec!(
    pub struct Ellipsoid<V>:UEllipsoid<V> {
        V pub dims,
    }

    fn dist(self, point: V) -> Self::Item {
        let k0 = (point/self.dims).length();
        let k1 = (point/(self.dims*self.dims)).length();
        k0*(k0-1.0)/k1
    }
);

sdf_impl!(
    pub struct Rhombus:URhombus {
        pub length: f32,
        pub width: f32,
        pub height: f32,
        pub corner_radius: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let point = point.abs();
        let b = vec2(self.length, self.height);
        let f = (ndot(b,b-2.0*point.xz())/b.dot(b)).clamp(-1.0 , 1.0);
        let q = vec2((point.xz()-0.5*b*vec2(1.0-f,1.0+f)).length()*(point.x*b.y+point.z*b.x-b.x*b.y).signum()-self.corner_radius, point.y-self.height);
        q.x.max(q.y).min(0.0) + q.max(Vec2::ZERO).length()
    }
);

sdf_impl!(
    pub struct Octahedron:UOctahedron {
        pub roundness: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let point = point.abs();
        let m = point.x+point.y+point.z-self.roundness;
        if 3.0*point.x < m {
            let q = point.xyz();
            let k = 0.5*(q.z-q.y+self.roundness).clamp(0.0, self.roundness);
            vec3(q.x, q.y-self.roundness+k,q.z-k).length()
        } else if 3.0*point.y < m {
            let q = point.yzx();
            let k = 0.5*(q.z-q.y+self.roundness).clamp(0.0, self.roundness);
            vec3(q.x, q.y-self.roundness+k,q.z-k).length()
        } else if 3.0*point.z < m {
            let q = point.zxy();
            let k = 0.5*(q.z-q.y+self.roundness).clamp(0.0, self.roundness);
            vec3(q.x, q.y-self.roundness+k,q.z-k).length()
        } else {
            m*0.57735027
        }
    }
);

sdf_impl!(
    pub struct Pyramid:UPyramid {
        pub height: f32,
    }

    fn dist(self, point: V) -> Self::Item {
        let m2 = self.height*self.height + 0.25;

        let point = Into::<V>::into((point.x.abs(), point.y, point.z.abs()));
        let xz = if point.z > point.x {point.zx()} else {point.xz()};
        let point = Into::<V>::into((xz.x - 0.5, point.y, xz.y - 0.5));

        let q = Into::<V>::into((point.z, self.height*point.y - 0.5*point.x, self.height*point.x + 0.5*point.y));

        let s = (-q.x).max(0.0);
        let t = ((q.y-0.5*point.z)/(m2+0.25)).clamp(0.0, 1.0);
        let a = m2*(q.x+s)*(q.x+s)+q.y*q.y;
        let b = m2*(q.x+0.5*t)*(q.x+0.5*t)+(q.y-m2*t)*(q.y-m2*t);

        let d2 = if q.y.min(-q.x*m2-q.y*0.5) > 0.0 {0.0} else {a.min(b)};

        (((d2+q.z*q.z)/m2)*q.z.max(-point.y).signum()).sqrt()
    }
);
