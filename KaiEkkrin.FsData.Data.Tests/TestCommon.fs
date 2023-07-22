module TestCommon

open FsCheck

// Generates a valid order for the tree (the B-value.)
// Whilst all values >= 3 are valid, not all are equally interesting and I want to bias the test set
// towards small and particular numbers. If these work, probably the rest should work...
let genBValue = Gen.elements [3; 4; 5; 6; 7; 8; 13; 21; 34; 99; 128]

