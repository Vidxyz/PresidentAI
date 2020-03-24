import game.{GameUtilities, Move, Round}
import org.scalatest.FunSpec

class RoundTest extends FunSpec{

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

  describe("tests for getIndexOf") {

  }

}
