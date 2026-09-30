var assert = require("assert");
var numbers = require("../index.js");
var basic = numbers.basic;

suite("numbers", () =>  {
  // longList is used by basic.max() and basic.min()
  // to test for `Maximum call stack size exceeded` exception.
  var longList = [],
    len = 1e7,
    sign;
  while (0 < len--) {
    sign = Math.random() < 0.5 ? -1 : 1;
    longList.push(sign * Math.floor(Math.random() * 1e5));
  }
  longList.push(1e6);
  longList.push(-1e6);
  Object.freeze(longList);

  // basic.sum
  test("sum should return the sum of items in an array", () => {
    assert.equal(basic.sum([0, 1, 2, 3]), 6);
    assert.equal(basic.sum([0, -3, 5, -2]), 0);
  });

  test("sum should throw an exception when given anything but an array", () => {
    assert.throws(() =>  {
      basic.sum(1);
    }, /Input must be of type Array/);
  });

  test("sum should throw an exception when given anything objects other than numbers", () => {
    assert.throws(() =>  {
      basic.sum([1, 2, "error"]);
    }, /All elements in array must be numbers/);
  });

  // basic.substraction
  test("subtraction should return the difference of items in an array", () => {
    assert.equal(basic.subtraction([0, 5, 3, 1, -9]), 0);
  });

  test("subtraction should throw an exception when given anything but an array", () => {
    assert.throws(() =>  {
      basic.subtraction(1);
    }, /Input must be of type Array/);
  });

  test("subtraction should throw an exception when given anything objects other than numbers", () => {
    assert.throws(() =>  {
      basic.subtraction(["test", 1, 1, 2]);
    }, /All elements in array must be numbers/);
  });

  test("subtraction should throw an exception last element is not a number", () => {
    assert.throws(() =>  {
      basic.subtraction([1, 1, 2, "test"]);
    }, /All elements in array must be numbers/);
  });

  // basic.product
  test("product should return the product of items in an array", () => {
    assert.equal(basic.product([1, 2, 3, 4]), 24);
    assert.equal(basic.product([-3, 2]), -6);
  });

  test("product should throw an exception when given anything but an array", () => {
    assert.throws(() =>  {
      basic.product(1);
    }, /Input must be of type Array/);
  });

  test("product should throw an exception when given anything objects other than numbers", () => {
    assert.throws(() =>  {
      basic.product([1, 2, "error"]);
    }, /All elements in array must be numbers/);
  });

  test("product should throw an exception when given anything objects other than numbers", () => {
    assert.throws(() =>  {
      basic.product(["error", 1, 2]);
    }, /All elements in array must be numbers/);
  });

  test("square should return the square of a number", () => {
    assert.equal(basic.square(4), 16);
  });

  test("square should throw an Error when input is other than a number", () => {
    assert.throws(() =>  {
      basic.square("error");
    }, /Input must be a number/);
  });

  // basic.binomial
  test("binomial should return the binomial coefficient (n choose k) of two numbers", () => {
    assert.equal(basic.binomial(5, 3), 10);
    assert.throws(() =>  {
      basic.binomial("error");
    }, /Input must be a number/);
  });

  // basic.factorial
  test("factorial should return the product of n * (n - 1) * (n - 2) * ... * 1", () => {
    assert.equal(basic.factorial(4), 24);
    assert.equal(basic.factorial(5), 120);
    assert.throws(
      () =>  {
        basic.factorial("error");
        basic.factorial(-1);
      },
      /Input must be a number/,
      /Input must not be negative/
    );
  });

  // basic.gcd
  test("gcd should throw an exception when given a decimal", () => {
    assert.throws(() =>  {
      basic.gcd(0.2, 1);
    }, "A or B are not integers");
  });
  test("gcd should return the greatest common denominator of two integers", () => {
    assert.equal(basic.gcd(1254, 0), 1254);
    assert.equal(basic.gcd(0, -5298), 5298);
    assert.equal(basic.gcd(0, -Infinity), Infinity);
    assert.equal(basic.gcd(4430, -Infinity), Infinity);
    assert.equal(basic.gcd(-1254, -5298), 6);
    assert.equal(basic.gcd(1254, 5298), 6);
    assert.equal(basic.gcd(78699786, 78978965), 1);
  });

  // basic.lcm
  test("lcm should return the least common multiple of two integers", () => {
    assert.equal(basic.lcm(4, 0), 0);
    assert.equal(basic.lcm(0, 4), 0);
    assert.equal(isNaN(basic.lcm(4, Infinity)), true);
    assert.equal(isNaN(basic.lcm(Infinity, 4)), true);
    assert.equal(basic.lcm(4, 5), 20);
    assert.equal(basic.lcm(3, 4), 12);
    assert.equal(basic.lcm(4, 6), 12);
    assert.equal(basic.lcm(21, 6), 42);
    assert.equal(basic.lcm(12, 80), 240);
  });

  // basic.max
  test("basic.max will throw an exception if argument is not an array.", () => {
    assert.throws(() =>  {
      basic.max(65, 40);
    }, /Input must be of type Array/);
  });

  test("max should return the biggest number in an array", () => {
    assert.equal(basic.max([1, 2, 3, 42]), 42);
    assert.equal(basic.max([-1, -2, -3, -42]), -1);
    assert.equal(basic.max([1, Infinity]), Infinity);
    assert.equal(basic.max(longList), 1000000);
  });

  // basic.min
  test("basic.min will throw an exception if argument is not an array.", () => {
    assert.throws(() =>  {
      basic.min(65, 40);
    }, /Input must be of type Array/);
  });

  test("min should return the smallest number in an array", () => {
    assert.equal(basic.min([1, 2, 3, 42]), 1);
    assert.equal(basic.min([-1, -2, -3, -42]), -42);
    assert.equal(basic.min([1, -Infinity]), -Infinity);
    assert.equal(basic.min(longList), -1000000);
  });

  // basic.range
  test("range should return an appropriate range for the given start, stop, and step parameters", () => {
    assert.deepEqual(basic.range(1, 10), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    assert.deepEqual(basic.range(10, 1), [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]);
    assert.deepEqual(
      basic.range(1, 5, 0.5),
      [1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5]
    );
    assert.deepEqual(
      basic.range(5, 1, 0.5),
      [5, 4.5, 4, 3.5, 3, 2.5, 2, 1.5, 1]
    );
  });

  // basic.isInt
  test("isInt checks for an integer", () => {
    assert.equal(basic.isInt(2.32), false);
    assert.equal(basic.isInt("true"), false);
    assert.equal(basic.isInt("2"), true); //based off impelementation change
    assert.equal(basic.isInt(2), true);
  });

  // basic.divMod
  test("divMod should return an array of both the division and modulus values of two integers", () => {
    assert.deepEqual(basic.divMod(12, 6), [2, 0]);
    assert.deepEqual(basic.divMod(10, 3), [3, 1]);
  });

  test("divMod should throw an exception when given a decimal", () => {
    assert.throws(() =>  {
      basic.divMod(0.2, 0.1);
    }, 'A or B are not integers');
  });

  // basic.egcd
  test("egcd should throw an exception when given a decimal", () => {
    assert.throws(() =>  {
      basic.egcd(0.2, 1);
    }, 'Inputs must be numbers.');
  });
  test("egcd should return the array [a, x, y] which is the solved linear equation for GCD", () => {
    assert.equal(basic.egcd("ten", 1).toString(), "NaN,NaN,NaN");
    assert.deepEqual(basic.egcd(1, Infinity), [Infinity, Infinity, Infinity]);
    assert.deepEqual(basic.egcd(3, 0), [3, 1, 0]);
    assert.deepEqual(basic.egcd(0, 3), [3, 0, 1]);
    assert.deepEqual(basic.egcd(-2, -6), [2, -1, 0]);
    assert.deepEqual(basic.egcd(-2, 5), [1, 2, 1]);
    assert.deepEqual(basic.egcd(65, 40), [5, -3, 5]);
    assert.deepEqual(basic.egcd(40, 65), [5, 5, -3]);
    assert.deepEqual(basic.egcd(1239, 735), [21, -16, 27]);
    assert.deepEqual(basic.egcd(105, 252), [21, 5, -2]);
    assert.deepEqual(basic.egcd(252, 105), [21, -2, 5]);
  });

  // basic.modInverse
  test("modInverse will return the modulo m inverse of a", () => {
    assert.equal(basic.modInverse(1, 5), 1);
  });

  test("modInverse will throw an exception if no modular inverse exists", () => {
    assert.throws(() =>  {
      basic.modInverse(65, 40);
    }, /No modular inverse exists/);
  });

  // basic.powerMod
  test("powerMod should return the answer to a^b mod m", () => {
    assert.equal(basic.powerMod(1, -1, 5), 1);
    assert.equal(basic.powerMod(2, 10, 3), 1);
    assert.equal(basic.powerMod(2, Math.pow(10, 9), 18), 16);
    assert.equal(basic.powerMod(6, 0.5, 10), 6);
    assert.equal(basic.powerMod(4, 13, 497), 445);
  });

  test("powerMod should throw an exception when given a non number", () => {
    assert.throws(() =>  {
      basic.powerMod("error", "error", "error");
    }, /Inputs must be numbers/);
  });

  test("should be able to check equality of two floating point numbers", () => {
    assert.equal(basic.numbersEqual(5, 5, numbers.EPSILON), true);
    assert.equal(basic.numbersEqual(5.0001, 5.0000001, numbers.EPSILON), true);
    assert.equal(basic.numbersEqual(-5, 5, numbers.EPSILON), false);
    assert.equal(basic.numbersEqual(5, 5.1, numbers.EPSILON), false);
    assert.equal(basic.numbersEqual(5, 5.001, numbers.EPSILON), false);
  });

  test("numbersEqual should throw an exception when given a non number", () => {
    assert.throws(() =>  {
      basic.powerMod("error", "error", 0.2);
    }, /Inputs must be numbers/);
  });

  // basic.fallingFactorial
  test("fallingFactorial should return correct answers", () => {
    var func = basic.fallingFactorial;

    assert.equal(func(0, 0), 1); //allows n=0
    assert.equal(func(7, 0), 1); //k = 0 returns 1.

    assert.equal(func(7, 7), 5040); //n=k returns n!
    assert.equal(func(7, 4), 840);

    assert.throws(
      () =>  {
        func(-2, 5);
        func(2, 4);
      },
      /negative/,
      /k is greater than n/
    );
  });

  // basic.permutation
  test("permutation should return the permutation coefficient (n permute k) of two numbers", () => {
    assert.equal(basic.permutation(5, 0), 1);
    assert.equal(basic.permutation(5, 1), 5);
    assert.equal(basic.permutation(5, 4), 120);
    assert.equal(basic.permutation(5, 5), 120);
  });
});
