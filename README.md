# dekrypto
A Krypto puzzle solver written in Haskell.

## What is Krypto?
> The Krypto deck consists of 56 cards: three of each of the numbers 1-6, four each of the numbers 7-10, two each of 11-17, one each of 18-25. Six cards are dealt: a common objective card at the top and five other cards below. Each player must use all five of the cards' numbers exactly once, using any combination of arithmetic operations (addition, subtraction, multiplication, and division), to form the objective card's number. The first player to come up with a correct formula is the winner.
> – [Wikipedia](https://en.wikipedia.org/wiki/Krypto_(game))

## Compile
<pre><b>$ </b>ghc --make dekrypto.hs</pre>

## Example Usage
<pre>
<b>$</b> ./dekrypto
<b>Cards (separated by spaces): </b>7 8 3 4 12
<b>Goal: </b>22
<b>7 + (3 * (8 - (12 / 4))) = 22</b>
<b>Solved in 0.191022 seconds</b>
</pre>
