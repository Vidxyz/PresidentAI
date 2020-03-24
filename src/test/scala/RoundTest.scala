import game.{DuplicatePlayerNamesException, GameUtilities, Move, Round}
import org.scalatest.FunSpec

class RoundTest extends FunSpec{

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

  }

  describe("tests for hasAlreadySkippedTurn") {

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

}
