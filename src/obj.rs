//! Objects for use with the [SDF] trait.
//! Objects describe a shape in space, for example
//! a [Sphere] or a [Box] as opposed to a transformation
//! upon an already existing object.
//!
//! Many of the Objects within this module can be created
//! by applying various operations to simpler shapes.
//!
//! For example an [Ellipsoid] is functionally an [Elongate]'d [Sphere]

use super::SDF;

use bevy::math::*;

fn ndot(a: Vec2, b: Vec2) -> f32 {
    a.x * b.x - a.y * b.y
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

        fn dist($Self:ident, $Point:ident: Vec3) -> Self::Item $Impl:expr
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
            /// Data provided by the user at creation.
            pub data: U,
        }

        impl $Struct {
            /// Creates a new [$Struct]
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

        impl SDF<()> for &$Struct {
            type Item = f32;
            /// Returns the distance between the given point
            /// and the surface of this [$Struct]
            fn dist($Self, $Point: Vec3) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3;
                $Impl
            }
        }

        impl<U> $UStruct<U> {
            /// Creates a new [$UStruct]
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

        impl<'a, U> SDF<U> for &'a $UStruct<U> {
            type Item = (f32, &'a U);
            /// Returns the distance between the given point
            /// and this [$UStruct]. Also returns the custom
            /// user data that was assigned at creation.
            fn dist($Self, $Point: Vec3) -> Self::Item {
                #[allow(dead_code)]
                type V = Vec3;
                ($Impl, &$Self.data)
            }
        }
    }
}

sdf_impl!(
    /// A Sphere centered at the origin defined by a radius.
    pub struct Sphere:USphere {
        /// The radius of the sphere.
        pub radius: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        point.length() - self.radius
    }
);

sdf_impl!(
    /// A Box centered at the origin.
    /// `dims` is the extents of the Box along each axis.
    /// This means that the box will be twice as long/tall/wide as `dims` appears to describe.
    /// A `dims` of (0.5, 0.5, 0.5) will create a 1x1x1 cube centered at the origin.
    pub struct Box:UBox {
        /// The X, Y, Z, extents of the Box.
        pub extents: Vec3,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let q = self.extents.abs() - point;
        (q.max(Vec3::ZERO) + q.max_element().min(0.0)).length()
    }
);

sdf_impl!(
    /// A Rounded Box centered at the origin.
    /// `dims` is the extents of the Box along each axis.
    /// `radius` is the radius of the curves along the edges of the box.
    /// The faces of the Box will still be at the extents.
    /// Extents means that the full dimensions of the box will be twice
    /// the value listed within the extents.
    pub struct RoundBox:URoundBox {
        /// The X, Y, Z, extents of the box.
        pub extents: Vec3,
        /// The radius of the rounded corners of the box.
        pub radius: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let q = self.extents.abs() - point;
        (q.max(Vec3::ZERO) + q.max_element().min(0.0)).length() - self.radius
    }
);

sdf_impl!(
    /// A Box Frame centered at the origin.
    /// `dims` describes the extents of the box.
    /// `thickness` describes the size of the solid portions of the frame.
    /// The edges of the box shape are the expanded internally towards the center
    /// in order to create the frame. Meaning that the shape will not extend past
    /// the described extents of the box.
    pub struct BoxFrame:UBoxFrame {
        /// The X, Y, Z, extents of the box.
        pub extents: Vec3,
        /// The size of the solid portions of the frame.
        pub thickness: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let dims = self.extents.abs() - point;
        let q = (dims+self.thickness).abs() - self.thickness;
        ((vec3(dims.x, q.y, q.z)).max(Vec3::ZERO)+q.y.max(q.z).max(dims.x).min(0.0)).length().min(
        ((vec3(q.x, dims.y, q.z)).max(Vec3::ZERO)+dims.y.max(q.z).max(q.x).min(0.0)).length()).min(
        ((vec3(q.x, q.y, dims.z)).max(Vec3::ZERO)+q.y.max(dims.z).max(q.x).min(0.0)).length())
    }
);

sdf_impl!(
    /// A Torus that is laying flat along the xz plane.
    /// `thickness` describes the distance of revolution that
    /// creates the torus.
    /// `radius` describes the radius of the circle that is revolved
    /// to create the torus.
    pub struct Torus:UTorus {
        /// The distance of revolution that creates the torus.
        /// Could be described as the 'width' of the torus.
        pub thickness: f32,
        /// The radius of the circle that is revolved to create
        /// the torus.
        pub radius: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let q = vec2(point.xz().length() - self.thickness, point.y);
        q.length() - self.radius
    }
);

sdf_impl!(
    /// A Capped Torus that is laying flat along the xz plane.
    /// `thickness` describes the distance of the revolution that
    /// creates the torus.
    /// `radius` describes the radius of the circle that is revolved
    /// to create the torus.
    /// `completeness` describes how much of the torus should be 'filled in'
    /// Values for completeness below 0 or above pi will cause strange behavior
    pub struct CappedTorus:UCappedTorus {
        /// The distance of the revolution that creates the torus.
        pub thickness: f32,
        /// The radius of the revolved circle to create the torus.
        pub radius: f32,
        /// How much of the torus is filled in, in radians.
        /// Values outside of the 0 - pi range will cause odd behavior.
        pub completeness: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let mut point = point;
        point.x = point.x.abs();
        let sc = vec2(self.completeness.sin(),self.completeness.cos());
        let k = if sc.y*point.x>sc.x*point.y { sc.dot(point.xy()) } else { point.xy().length() };
        ((point.dot(point) + self.radius*self.radius - 2.0*self.radius*k) - self.radius).sqrt()
    }
);

sdf_impl!(
    /// A 'link' shape, essentially created by elongating a Torus along the Y
    /// axis.
    /// `length` describes the amount of elongation. A value of
    /// 0 will create a Torus.
    /// `thickness` describes the width of the overall shape.
    /// `radius` describes the width of the actual solid part of the shape.
    pub struct Link:ULink {
        /// The elongation of the original Torus.
        pub length: f32,
        /// The width of the overall shape.
        pub thickness: f32,
        /// The width of the solid part of the shape.
        pub radius: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let q = vec3(point.x, (point.y.abs() - self.length).max(0.0), point.z);
        vec2(q.xy().length() - self.thickness,q.z).length() - self.radius
    }
);

sdf_impl!(
    /// An Infinite Cylinder along the Y axis.
    /// `radius` is the radius of the cylinder itself.
    pub struct ICylinder:UICylinder {
        /// The radius of the cylinder.
        pub radius: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        point.xz().length() - self.radius
    }
);

sdf_impl!(
    /// A Cone oriented along the Y axis.
    /// `height` is the height of the cone.
    /// `angle` is the angle of the cone,
    /// values outside of 0 to pi will cause strange
    /// behavior.
    pub struct Cone:UCone {
        /// The height of the cone.
        pub height: f32,
        /// The angle of the cone.
        pub angle: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let c = vec2(self.angle.sin(), self.angle.cos());
        let q = point.xz().length();
        c.dot(vec2(q, point.y)).max(-self.height-point.y)
    }
);

sdf_impl!(
    /// A hexagonal prism oriented along the Y axis.
    /// `height` is the halved height of the prism, as
    /// it is used as an extent and thus extends both
    /// above and below the prism.
    /// `length` is the prism length.
    pub struct HexPrism:UHexPrism {
        /// The vertical extent of the prism.
        pub height: f32,
        /// The length of the prism.
        pub length: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let k = vec3(-0.8660254, 0.5, 0.57735);
        let point = point.abs();
        let point: Vec3 = (point.xy() - 2.0*k.xy().dot(point.xy()).min(0.0), point.z).into();
        let d = vec2(
            (point.xy()-vec2(point.x.clamp(-k.z*self.length, k.z*self.length), self.length)).length()*(point.y-self.length).signum(),
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

    fn dist(self, point: Vec3) -> Self::Item {
        let q = point.abs();
        (q.z-self.height).max((q.x*0.866025+point.y*0.5).max(-point.y)-self.radius*0.5)
    }
);

sdf_impl!(
    pub struct Capsule:UCapsule {
        pub height: f32,
        pub radius: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
        let point = vec3(point.x, point.y - point.y.clamp(0.0, self.height), point.z);
        point.length() - self.radius
    }
);

sdf_impl!(
    pub struct Cylinder:UCylinder {
        pub height: f32,
        pub radius: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
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

    fn dist(self, point: Vec3) -> Self::Item {
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

    fn dist(self, point: Vec3) -> Self::Item {
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

    fn dist(self, point: Vec3) -> Self::Item {
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

    fn dist(self, point: Vec3) -> Self::Item {
        let w = (self.radius*(self.radius-self.height)*self.height).sqrt();

        let q = vec2(point.xz().length(), point.y);
        let s = (self.height - self.radius)*q.x*q.x+w*w*(self.height+self.radius-2.0*q.y).max(self.height*q.x-w*q.y);
        if s < 0.0 {
            q.length()-self.radius
        } else if q.x<w {
            self.height - q.y
        } else {
            (q-vec2(w,self.height)).length()
        }
    }
);

sdf_impl!(
    pub struct CutHollowSphere:UCutHollowSphere {
        pub radius: f32,
        pub height: f32,
        pub thickness: f32,
    }

    fn dist(self, point: Vec3) -> Self::Item {
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

    fn dist(self, point: Vec3) -> Self::Item {
        let b = (self.base_radius - self.tip_radius)/self.height;
        let a = (1.0-b*b).sqrt();

        let q = vec2(point.xz().length(), point.y);
        let k = q.dot(vec2(-b, a));
        if k < 0.0 { q.length() -self.base_radius }
        else if k > a*self.height {(q-vec2(0.0,self.height)).length() - self.tip_radius}
        else { q.dot(vec2(a,b)) - self.base_radius }
    }
);

sdf_impl!(
    pub struct Ellipsoid:UEllipsoid {
        pub dims: Vec3,
    }

    fn dist(self, point: Vec3) -> Self::Item {
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

    fn dist(self, point: Vec3) -> Self::Item {
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

    fn dist(self, point: Vec3) -> Self::Item {
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

    fn dist(self, point: Vec3) -> Self::Item {
        let m2 = self.height*self.height + 0.25;

        let point = vec3(point.x.abs(), point.y, point.z.abs());
        let xz = if point.z > point.x {point.zx()} else {point.xz()};
        let point = vec3(xz.x - 0.5, point.y, xz.y - 0.5);

        let q = vec3(point.z, self.height*point.y - 0.5*point.x, self.height*point.x + 0.5*point.y);

        let s = (-q.x).max(0.0);
        let t = ((q.y-0.5*point.z)/(m2+0.25)).clamp(0.0, 1.0);
        let a = m2*(q.x+s)*(q.x+s)+q.y*q.y;
        let b = m2*(q.x+0.5*t)*(q.x+0.5*t)+(q.y-m2*t)*(q.y-m2*t);

        let d2 = if q.y.min(-q.x*m2-q.y*0.5) > 0.0 {0.0} else {a.min(b)};

        (((d2+q.z*q.z)/m2)*q.z.max(-point.y).signum()).sqrt()
    }
);
