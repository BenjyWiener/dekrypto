# dekrypto
A Krypto puzzle solver written in Haskell.

## What is Krypto?
> The Krypto deck consists of 56 cards: three of each of the numbers 1-6, four each of the numbers 7-10, two each of 11-17, one each of 18-25. Six cards are dealt: a common objective card at the top and five other cards below. Each player must use all five of the cards' numbers exactly once, using any combination of arithmetic operations (addition, subtraction, multiplication, and division), to form the objective card's number. The first player to come up with a correct formula is the winner.
> – [Wikipedia](https://en.wikipedia.org/wiki/Krypto_(game))

## Compile
<pre><b>$ </b>ghc -O2 --make dekrypto</pre>

## Usage
- **\*nix**:  <pre><b>... $</b> ./dekrypto [-H|-h]</pre>

- **Windows**:  <pre><b>C:\\...></b> dekrypto.exe [-H|-h]</pre>

<pre>
-H : Use house rules (allow intermediate results to be negative and/or
     fractional)
-h : Show the help message
</pre>

###### Examples

<pre>
<b>... $</b> ./dekrypto
<b>Cards (separated by spaces): </b>1 4 7 10 13
<b>Goal: </b>9
<b>1 + (4 + (7 - (13 - 10))) = 9</b>
<b>Solved in 0.01258 seconds</b>
</pre>

<pre>
<b>... $</b> ./dekrypto -H
<b>Cards (separated by spaces): </b>1 4 7 10 13
<b>Goal: </b>9
<b>1 + (4 + (7 + (10 - 13))) = 9</b>
<b>Solved in 0.000326 seconds</b>
</pre>
