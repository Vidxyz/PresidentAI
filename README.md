# President AI

This project is a scala-swing GUI based game that allows you to play the game of president against the computer.

This game allows you to choose between 2-6 players to play the game


## Getting Started
```sbt clean compile``` 

``` sbt run```


## Prerequisites
- Scala 2.13
- Jacoco (test coverage)
- Scala Swing


## Testing the project
- To run all unit tests:  
```
$ sbt test
```
- To run unit tests with a summary of code coverage
```
$ sbt jacoco
```


## Rules of the game

- Aim of the game is to lose all your cards as quickly as possible
- Each game comprises of multiple *rounds*. Rounds can be arbitrarily long or short depending on cards involved
- You can lose cards by playing a move on top of the current round state
    - Playing a move can either lead to the round ending, or continuing
    - Moves comprise of a single card, or multiple cards of the same face value
    - If a move **burns** the game state, then the round ends
    - A **burn** happens when a move's highest card comprises of a suit that is **better** than the highest card of the current game state
    - A burn clears the game state, and allows the person who burnt the round to begin a fresh one
         - It is thus advantageous to burn as much as possible
    - The ordering of suits is as follows - **SPADES** > **HEARTS** > **CLUBS** > **DIAMONDS**
- Cards are classified as either NormalCards, WildCards, SpecialCards or Jokers
 - Jokers are the ultimate trump card, they can be played at any time
 - 2s can be played to *contest* the round at any timie
   - Suit burns apply to 2s as well
   - You need to play ```max(1, n-1)``` 2s if the game state parity is ```n```
   - Game state parity is defined as number of cards played in the last valid round move
 - 3s are wildcards - they assume any value between 4-ACE of the same suit
   - For example, playing `3_Spade, King_Diamond` is equivalent to playing `King_Spade, King_Diamonod`
   - The real card can however burn the wildcard assuming its value!
   - For example, `3_spade, King_Diamond` can be **burned** by `King_Diamond, King_Spade`
 - All other cards are NormalCards - and they just assume the value/suit shown on its face 
- Example 1 :-  
    - The game state is empty (you are starting the round) - you can play anything
    - If the game state is non empty - lets say it comprises of two 4s - `4_clubs, 4_hearts`
    - Valid moves include 
        - Any double higher than double 4s
        - `4_diamond, 4_spade` (since **SPADE** > **HEART**)
        - A single 2
        - A single Joker
- Example 2 :- 
    - If the game state is comprised of `3_Spade, 4_Diamond, 4_Heart`
    - Valid moves include
        - Any triple higher than triple 4s - it could involve the use of a WildCard
        - `3_any, 4_Club, 4_Spade` - since `4_Spade` > `3_Spade` assuming face-value of 4
        -  Two 2s - `2_diamond, 2_spade` for example
        - A single Joker
- The game continues on until there is only one person remaining
    - This person then becomes **Bum**
- The person to lose all their cards first becomes **President**
- Everyone else finishes at positions between these two, based on order of completion
- All the positions, in order, are as follows
    - President
    - Vice-President
    - Neutral
    - Vice-Bum
    - Bum
- President gets Bum's 
    - Best card, if there are <= 3 players in the game
    - Two best cards, if there are >= 4 players in the game
    - President also gets to start the next game
- Vice-President gets Vice-Bum's one best card
- Neutral gets no cards and loses no cards
- Vice-Bum is forced to give up their best card to the Vice-President
- Bum is forced to give up to the President
    - Their best card, if there are `<= 3` players in the game
    - Their two best cards, if there are `>= 4` players in the game

