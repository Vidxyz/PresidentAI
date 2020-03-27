package game

import player.Player

case class Round(gameState: Move,
                 lastMovePlayedBy: String,
                 totalNumberOfPlayers: Int,
                 currentPlayerTurn: Int,
                 listOfPlayers: List[Player],
                 roundPassStatus: List[Boolean]) {

  /* Gets the index of the NEXT player who is supposed to play.
     Defined by the original list of players name
    */
  def getIndexOfNextPlayer: Int = {
    val originalIndexOfPlayerWhoPlayedLastMove: Int = Round.initialListOfPlayerNames
      .zipWithIndex
      .filter {
        case (name, _) => name == lastMovePlayedBy
      }
      .map {
        case (_, index) => index
      }
      .head

    var nextPlayerOriginalIndex = if(originalIndexOfPlayerWhoPlayedLastMove + 1 == Round.initialListOfPlayerNames.size) 0
                                  else originalIndexOfPlayerWhoPlayedLastMove + 1
    var playerSupposedToPlayNext: String = Round.initialListOfPlayerNames(nextPlayerOriginalIndex)

    while(!listOfPlayers.map(p => p.name).contains(playerSupposedToPlayNext)) {
       nextPlayerOriginalIndex = if(nextPlayerOriginalIndex + 1 == Round.initialListOfPlayerNames.size) 0
                                 else nextPlayerOriginalIndex + 1
      playerSupposedToPlayNext = Round.initialListOfPlayerNames(nextPlayerOriginalIndex)
    }

    getIndexOf(playerSupposedToPlayNext)

  }

  /*
  Return TRUE if everyone has passed, except for the person who played the last move
  Returns FALSE otherwise
   */
  def hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed: Boolean = {
    (listOfPlayers zip roundPassStatus)
      .filter(tuple => tuple match {
        case (player, _passStatus) => !checkIfLastMovePlayedBy(player.name)
      })
      .foldLeft(true)((acc, tuple1) => acc && tuple1._2)
  }

  def checkIfLastMovePlayedBy(name: String): Boolean = name == lastMovePlayedBy

  /*
  Returns True if round.lastMovePlayedBy is not present in round.listOfPlayers. False otherwise
  Only exception to this is when lastMovePlayedBy is empty string, since that is the start case
   */
  def playerEndedTheGameOnABurn: Boolean =
    !this.listOfPlayers.map(player => player.name).contains(this.lastMovePlayedBy) && this.lastMovePlayedBy != ""

  /*
  Since name is unique, this should only return a list of size 1
  Assumes supplied name is part of listOfPlayers. Throws exception otherwise
   */
  def hasAlreadySkippedTurn(name: String): Boolean = {
    (listOfPlayers zip roundPassStatus)
      .filter(tuple => tuple match {
        case (player, _passStatus) => player.name == name
      })
      .map { case (_, status) => status }
      .head
  }

  /*
 Since name is unique, this should only return a list of size 1
 Assumes that name is present in list
  */
  def getIndexOf(name: String): Int = {
    listOfPlayers
      .zipWithIndex
      .filter {
        case (player, _) => player.name == name
      }
      .map {
        case (_, index) => index
      }
      .head
  }

  /*
  Updated roundPassStatus having removed the element at the index specified
  This is used when a player has exited the game and everything needs to be augmented
   */
  def updatedRoundPassStatus(indexToRemove: Int): List[Boolean] = {
    val tempBuffer = roundPassStatus.toBuffer
    tempBuffer.remove(indexToRemove)
    tempBuffer.toList
  }

}

object Round {
  def getNoPassList(numberOfPlayers: Int): List[Boolean] = {
    (1 to numberOfPlayers).toList.map(_ => false)
  }

  /* Maintains the initial list of playernames for future tracking */
  var initialListOfPlayerNames: List[String] = List.empty

  /*
  Throws exception if player names are not unique
  Returns new round object otherwise
   */
  def apply(gameState: Move, lastMovePlayedBy: String, totalNumberOfPlayers: Int, currentPlayerTurn: Int,
            listOfPlayers: List[Player], roundPassStatus: List[Boolean]): Round = {
    if(listOfPlayers.size - listOfPlayers.map(p => p.name).distinct.size != 0) throw DuplicatePlayerNamesException("Player names must be unique")
    else new Round(gameState, lastMovePlayedBy, totalNumberOfPlayers, currentPlayerTurn, listOfPlayers, roundPassStatus)
  }

}

case class DuplicatePlayerNamesException(s: String) extends IllegalStateException(s)

