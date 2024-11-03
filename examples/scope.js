let a = 5;
let b = 6;

console.log(a + b);

{
    let a = "runaway";
    let b = "horses";
    console.log(a + " " + b);
}

{
    b = 5;
    let c = 5;
    console.log(a + b + c);
}

console.log(a + b);


var AH = "outer";

{
    let AH = "inner";
    console.log(AH);
}