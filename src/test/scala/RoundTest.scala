import game.{DuplicatePlayerNamesException, GameUtilities, Move, Round}
import org.scalatest.FunSpec

class RoundTest extends FunSpec {

  describe("tests for apply method") {
    it("Should throw exception when player names are repeated") {
      val players = GameUtilities.generatePlayersAndDealHands(List("p1", "p2", "p1"))
      val gameState = Move(List.empty)
      val roundPassStatus = Round.getNoPassList(players.size)
      assertThrows[DuplicatePlayerNamesException](Round(gameState, "p2", players.size, 0, players, roundPassStatus))
    }

    it("Should not throw exception when player names are not repeated") {
      val players = GameUtilities.generatePlayersAndDealHands(List("p1", "p2", "p3"))
      val gameState = Move(List.empty)
      val roundPassStatus = Round.getNoPassList(players.size)
      val result = Round(gameState, "p2", players.size, 0, players, roundPassStatus)
      assert(result.gameState == gameState)
      assert(result.lastMovePlayedBy == "p2")
      assert(result.totalNumberOfPlayers == players.size)
      assert(result.currentPlayerTurn == 0)
      assert(result.listOfPlayers == players)
      assert(result.roundPassStatus == Round.getNoPassList(players.size))
    }
  }

  describe("tests for hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed()") {
    val gameState = Move(List.empty)
    val players = GameUtilities.generatePlayersAndDealHands(List("p1", "p2", "p3", "p4"))

    describe("When everyone has passed already") {
      it("Should return true") {
        val roundPassStatus = List(true, true, true, true)
        val round = Round(gameState, "p3", players.size, 1, players, roundPassStatus)
        assert(round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed)
      }
    }

    describe("When everyone except for the player who played last has passed already") {
      it("Should return true") {
        val roundPassStatus = List(true, true, false, true)
        val round = Round(gameState, "p3", players.size, 1, players, roundPassStatus)
        assert(round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed)
      }
    }

    describe("When everyone except for the player who didn't play last has passed already") {
      it("Should return false") {
        val roundPassStatus = List(true, false, true, true)
        val round = Round(gameState, "p3", players.size, 1, players, roundPassStatus)
        assert(!round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed)
      }
    }

    describe("When not everyone has passed") {
      it("Should return false") {
        val roundPassStatus = List(false, false, true, true)
        val round = Round(gameState, "p3", players.size, 0, players, roundPassStatus)
        assert(!round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed)
      }
    }

    describe("When not everyone has passed, including the player who played last") {
      it("Should return false") {
        val roundPassStatus = List(false, false, false, true)
        val round = Round(gameState, "p3", players.size, 0, players, roundPassStatus)
        assert(!round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed)
      }
    }

    describe("When not everyone has passed, except the player who played last") {
      it("Should return false") {
        val roundPassStatus = List(false, false, true, false)
        val round = Round(gameState, "p3", players.size, 0, players, roundPassStatus)
        assert(!round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed)
      }
    }

  }

  describe("tests for checkIfLastMovePlayedBy()") {
    val gameState = Move(List.empty)
    val listOfPlayers = GameUtilities.generatePlayersAndDealHands(List("p1", "p2"))
    val roundPassStatus = Round.getNoPassList(listOfPlayers.size)
    val round = Round(gameState, "p2", listOfPlayers.size, 0, listOfPlayers, roundPassStatus)

    it("Should return true when parameter is p2") {
      assert(round.checkIfLastMovePlayedBy("p2"))
    }

    it("Should return false when parameter is p1") {
      assert(!round.checkIfLastMovePlayedBy("p1"))
    }

    it("Should return false when parameter is empty string") {
      assert(!round.checkIfLastMovePlayedBy(""))
    }

    it("Should return false if parameter supplied is not included in list of names") {
      assert(!round.checkIfLastMovePlayedBy("p3"))
    }

  }

  describe("tests for playerEndedTheGameOnABurn()") {
    val gameState = Move(List.empty)
    val listOfPlayers = GameUtilities.generatePlayersAndDealHands(List("p1", "p2", "p3", "p4"))
    val roundPassStatus = Round.getNoPassList(listOfPlayers.size)

    describe("When listOfPlayers does not contain the name of lastMovePlayedBy") {
      it("Should return true when lastMovePlayedBy is an actual name") {
        val round = Round(gameState, "p5", listOfPlayers.size, 0, listOfPlayers, roundPassStatus)
        assert(round.playerEndedTheGameOnABurn)
      }

      it("Should return false when lastMovePlayedBy is an empty string") {
        val round = Round(gameState, "", listOfPlayers.size, 0, listOfPlayers, roundPassStatus)
        assert(!round.playerEndedTheGameOnABurn)
      }
    }

    describe("When listOfPlayers contains the name of lastMovePlayedBy") {
      it("Should return false, since the player is still present in the game") {
        val round = Round(gameState, "p2", listOfPlayers.size, 0, listOfPlayers, roundPassStatus)
        assert(!round.playerEndedTheGameOnABurn)
      }
    }
  }

  describe("tests for hasAlreadySkippedTurn()") {
    val gameState = Move(List.empty)
    val listOfPlayers = GameUtilities.generatePlayersAndDealHands(List("p1", "p2", "p3", "p4"))

    describe("When player has already skipped turn this round") {
      it("Should return true"){
        val roundPassStatus = List(false, true, false, false)
        val round = Round(gameState, "p2", listOfPlayers.size, 0, listOfPlayers, roundPassStatus)
        assert(round.hasAlreadySkippedTurn("p2"))
      }
    }

    describe("When player has not skipped turn this round") {
      it("Should return false"){
        val roundPassStatus = List(false, false, true, true)
        val round = Round(gameState, "p2", listOfPlayers.size, 0, listOfPlayers, roundPassStatus)
        assert(!round.hasAlreadySkippedTurn("p2"))
      }
    }
  }

  describe("tests for getIndexOf()") {
    val gameState = Move(List.empty)
    val listOfPlayers = GameUtilities.generatePlayersAndDealHands(List("p1", "p2", "p3", "p4", "p5"))
    val roundPassStatus = Round.getNoPassList(listOfPlayers.size)
    val round = Round(gameState, "p2", listOfPlayers.size, 0, listOfPlayers, roundPassStatus)

    it("Should return 0 when parameter is p1") {
      assert(round.getIndexOf("p1") == 0)
    }

    it("Should return 4 when parameter is p5") {
      assert(round.getIndexOf("p5") == 4)
    }

    it("Should return 2 when parameter is p3") {
      assert(round.getIndexOf("p3") == 2)
    }

    it("Should throw an exception when supplied name isn't part of the round players") {
      assertThrows[Exception](round.getIndexOf("potato"))
    }

  }

  describe("tests for getIndexOfNextPlayer()") {
    Round.initialListOfPlayerNames = List("p1", "p2", "p3", "p4")
    val players = GameUtilities.generatePlayersAndDealHands(Round.initialListOfPlayerNames)

    describe("When the player who's turn it is exists still") {

      it("Should return their index when it is at the start of the list") {
        val round = Round(Move(List.empty), "p1", 4, 3, players, Round.getNoPassList(4))
        assert(round.getIndexOfNextPlayer == 1)
      }

      it("Should return index when it is at the end of the list") {
        val round = Round(Move(List.empty), "p4", 4, 3, players, Round.getNoPassList(4))
        assert(round.getIndexOfNextPlayer == 0)
      }

      it("Should return index when it is in the middle of the list") {
        val round = Round(Move(List.empty), "p2", 4, 3, players, Round.getNoPassList(4))
        assert(round.getIndexOfNextPlayer == 2)
      }

    }

    describe("When the player who's turn it is does not exist anymore") {

      describe("When only one player has completed the game") {

        describe("When the player who completed started the game") {
          it("Should return index 0") {
            val round = Round(Move(List.empty), "p1", 4, 3, players.drop(1), Round.getNoPassList(4))
            assert(round.getIndexOfNextPlayer == 0)
          }
        }

        describe("When the player who completed went last in the first round") {
          it("Should return index 0") {
            val round = Round(Move(List.empty), "p4", 4, 3, players.take(3), Round.getNoPassList(4))
            assert(round.getIndexOfNextPlayer == 0)
          }
        }

        describe("When the player who completed went 2nd in the first round") {
          it("Should return index 0") {
            val round = Round(Move(List.empty), "p2", 4, 3, players.slice(0,1) ++ players.slice(2, 4), Round.getNoPassList(4))
            assert(round.getIndexOfNextPlayer == 1)
          }
        }

      }

      describe("When two players have completed the game") {
        describe("When the player who completed most recently is p2 - after p3") {
          it("Should return index 1 pertaining to p4") {
            val round = Round(Move(List.empty), "p2", 4, 3, players.slice(0,1) ++ players.slice(3, 4), Round.getNoPassList(4))
            assert(round.getIndexOfNextPlayer == 1)
          }
        }

        describe("When the player who completed most recently is p4 - after p3") {
          it("Should return index 0 pertaining to p1") {
            val round = Round(Move(List.empty), "p4", 4, 3, players.slice(0,2), Round.getNoPassList(4))
            assert(round.getIndexOfNextPlayer == 0)
          }
        }

        describe("When the player who completed most recently is p1 - after p3") {
          it("Should return index 0 pertaining to p2") {
            val round = Round(Move(List.empty), "p1", 4, 3, players.slice(1,2) ++ players.slice(3,4), Round.getNoPassList(4))
            assert(round.getIndexOfNextPlayer == 0)
          }
        }

        describe("When the player who completed most recently is p3 - after p2") {
          it("Should return index 1 pertaining to p4") {
            val round = Round(Move(List.empty), "p1", 4, 3, players.slice(0,1) ++ players.slice(3,4), Round.getNoPassList(4))
            assert(round.getIndexOfNextPlayer == 1)
          }
        }
      }

    }

  }

  describe("tests for updatedRoundPassStatus") {
    Round.initialListOfPlayerNames = List("p1", "p2", "p3", "p4")
    val players = GameUtilities.generatePlayersAndDealHands(Round.initialListOfPlayerNames)

    describe("When index is at the beginning") {
      it("Should return expected value") {
        val round = Round(Move(List.empty), "p2", 4, 3, players, List(true, false, true, false))
        val expected = List(false, true, false)
        assert(round.updatedRoundPassStatus(0) == expected)
      }
    }

    describe("When indexToRemove is at the end") {
      it("Should return expected value") {
        val round = Round(Move(List.empty), "p2", 4, 3, players, List(true, false, true, false))
        val expected = List(true, false, true)
        assert(round.updatedRoundPassStatus(3) == expected)
      }
    }

    describe("When indexToRemove is in the middle") {
      it("Should return expected value") {
        val round = Round(Move(List.empty), "p2", 4, 3, players, List(true, false, true, false))
        val expected = List(true, true, false)
        assert(round.updatedRoundPassStatus(1) == expected)
      }
    }

    describe("When indexToRemove is greater than size of roundPassStatus") {
      it("Should return same value") {
        val round = Round(Move(List.empty), "p2", 4, 3, players, List(true, false, true, false))
        val expected = List(true, false, true, false)
        assertThrows[IndexOutOfBoundsException](round.updatedRoundPassStatus(5) == expected)
      }
    }

  }

}
