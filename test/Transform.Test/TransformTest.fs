/// Copyright (C) 2016 The Authors.
module Transform.Test

open Xunit;
open FsUnit.Xunit;

[<Fact>]
let ``empty constructs an identity transformation`` () =
    let t = empty

    // Check that a transformation was constructed
    t |> should be instanceOfType<Transformation>

[<Fact>]
let ``translate constructs a translation transformation`` () =
    let t = translate 1. 2. 3.

    let p = Point.make 3. 4. 5.
    let q = Point.make 4. 6. 8.

    let v = Vector.make 3. 4. 5.

    // Points are affected by translations...
    p * t |> should equal q

    // ...but vectors are not.
    v * t |> should equal v

[<Fact>]
let ``scale constructs a scaling transformation`` () =
    let t = scale 2. 3. 4.

    let p = Point.make 3. 4. 5.
    let q = Point.make 6. 12. 20.

    let v = Vector.make 3. 4. 5.
    let u = Vector.make 6. 12. 20.

    // Both points and vectors are affected by scaling.
    p * t |> should equal q
    v * t |> should equal u

[<Fact>]
let ``mirror constructs a mirroring transformation`` () =
    let tx = mirror X
    let ty = mirror Y
    let tz = mirror Z

    let p = Point.make 3. 4. 5.
    let qx = Point.make -3. 4. 5.
    let qy = Point.make 3. -4. 5.
    let qz = Point.make 3. 4. -5.

    p * tx |> should equal qx
    p * ty |> should equal qy
    p * tz |> should equal qz

    let v = Vector.make 3. 4. 5.
    let ux = Vector.make -3. 4. 5.
    let uy = Vector.make 3. -4. 5.
    let uz = Vector.make 3. 4. -5.

    v * tx |> should equal ux
    v * ty |> should equal uy
    v * tz |> should equal uz

[<Fact>]
let ``rotate constructs a rotation transformation`` () =
    // The angle to rotate; 90 degrees.
    let a = System.Math.PI / 2.

    let tx = rotate X a
    let ty = rotate Y a
    let tz = rotate Z a

    let p = Point.make 3. 4. 5.
    let qx = Point.make 3. -5. 4.
    let qy = Point.make 5. 4. -3.
    let qz = Point.make -4. 3. 5.

    for (t, q) in [(tx, qx); (ty, qy); (tz, qz)] do
        Point.getX (p * t) |> should (equalWithin 0.001) (Point.getX q)
        Point.getY (p * t) |> should (equalWithin 0.001) (Point.getY q)
        Point.getZ (p * t) |> should (equalWithin 0.001) (Point.getZ q)

    let v = Vector.make 3. 4. 5.
    let ux = Vector.make 3. -5. 4.
    let uy = Vector.make 5. 4. -3.
    let uz = Vector.make -4. 3. 5.

    for (t, u) in [(tx, ux); (ty, uy); (tz, uz)] do
        Vector.getX (v * t) |> should (equalWithin 0.001) (Vector.getX u)
        Vector.getY (v * t) |> should (equalWithin 0.001) (Vector.getY u)
        Vector.getZ (v * t) |> should (equalWithin 0.001) (Vector.getZ u)

[<Fact>]
let ``transpose tranposes a transformation`` () =
    let t = translate 1. 2. 3.

    // A transformation will return to its original state if transposed twice.
    t |> should not' (equal (transpose t))
    t |> should equal (transpose (transpose t))

[<Fact>]
let ``inserve inverses a transformation`` () =
    let t = translate 1. 2. 3.
    let s = scale 1. 2. 3.
    let m = mirror X
    let r = rotate X System.Math.PI
    let h = shear X Y 2.

    // A transformation multiplied by its inverse is the identity transformation.
    for t in [t; s; m; r; h] do
        t * inverse t |> should equal empty

[<Fact>]
let ``merge concatenates a list of transformations`` () =
    let t = translate 1. 2. 3.
    let s = scale 1. 2. 3.
    let m = mirror X

    // Merging is the same as multiplying the individual transformations.
    merge [t; s; m] |> should equal (t * s * m)
