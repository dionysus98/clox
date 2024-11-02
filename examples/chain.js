function makeCounter() {
    let i = 0;
 
    function count() {
        i = i + 1;
        console.log(i);
    }

    return count;
}

const counter = makeCounter();
counter(); // "1".
counter(); // "2".
