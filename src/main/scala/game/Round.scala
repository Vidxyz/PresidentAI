package game

import player.Player

case class Round(gameState: Move,
                 lastMovePlayedBy: String,
                 currentPlayerTurn: Int,
                 listOfPlayers: List[Player],
                 roundPassStatus: Map[String, Boolean],
                 movesPlayed: List[Move] = List.empty) {

  def getNextActivePlayerInSequence(currentPlayerIndex: Int): Int = {
    var nextIndex = if(currentPlayerIndex + 1 == listOfPlayers.size) 0 else currentPlayerIndex + 1
    var nextPlayer = listOfPlayers(nextIndex)
    while(nextPlayer.status == Complete) {
      nextIndex = if(nextIndex + 1 == listOfPlayers.size) 0 else nextIndex + 1
      nextPlayer = listOfPlayers(nextIndex)
    }
    nextIndex
  }

  /* Used when round.lastMovePlayedBy is played by someone who does not exist anymore (completed)
     Gets the index of the NEXT player who is supposed to play.
     Defined by the original list of players name
    */
  def getIndexOfNextPlayer: Int = {
    val indexOfPlayerWhoPlayedLastMove: Int = listOfPlayers.map(p => p.name)
      .zipWithIndex
      .filter {
        case (name, _) => name == lastMovePlayedBy
      }
      .map {
        case (_, index) => index
      }
      .head

    var nextPlayerIndex = if(indexOfPlayerWhoPlayedLastMove + 1 == listOfPlayers.size) 0
                                  else indexOfPlayerWhoPlayedLastMove + 1
    var playerSupposedToPlayNext: String = listOfPlayers.map(_.name)(nextPlayerIndex)

    // Keep trying until player index is an Active player who is supposed to play next
    while(!listOfPlayers.filter(p => p.status == Active).map(_.name).contains(playerSupposedToPlayNext)) {
       nextPlayerIndex = if(nextPlayerIndex + 1 == listOfPlayers.size) 0
                                 else nextPlayerIndex + 1
      playerSupposedToPlayNext = listOfPlayers.map(_.name)(nextPlayerIndex)
    }

    nextPlayerIndex
  }

  /*
  Return TRUE if everyone has passed, except for the person who played the last move
  Returns FALSE otherwise
  Assumption - RoundPassStatus only has statuses of those who are active, no one else
   */
  def hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed: Boolean = {
    roundPassStatus.filter(x => !checkIfLastMovePlayedBy(x._1)).toList.foldLeft(true)((acc, tuple) => acc && tuple._2)
  }

  def checkIfLastMovePlayedBy(name: String): Boolean = name == lastMovePlayedBy

  /*
  Returns True if `round.lastMovePlayedBy` is not present in round.listOfPlayers under an active player
  False otherwise
  Only exception to this is when lastMovePlayedBy is empty string, since that is the start case
   */
  def playerEndedTheGameOnABurn: Boolean =
    !this.listOfPlayers.filter(p => p.status == Active).map(player => player.name).contains(this.lastMovePlayedBy) && this.lastMovePlayedBy.nonEmpty

  /*
  Since name is unique, this should only return a list of size 1
  Assumes supplied name is part of listOfPlayers. Throws exception otherwise
   */
  def hasAlreadySkippedTurn(name: String): Boolean = {
    roundPassStatus.filter(tuple => tuple._1 == name).toList.map(tuple => tuple._2).head
  }

  /*
 Since name is unique, this should only return a list of size 1
 Assumes that name is present and active, throws exception otherwise
  */
  def getIndexOf(name: String): Int = {
    listOfPlayers
      .zipWithIndex
      .filter {
        case (player, _) => player.name == name && player.status == Active
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
  def updatedRoundPassStatus(name: String): Map[String, Boolean] = {
    roundPassStatus - name
  }

}

object Round {
  def getPassStatusFalseForAll(players: List[Player]): Map[String, Boolean] = {
    players.filter(_.status == Active).map(_.name).map(t => t -> false).toMap
  }

  /*
  Throws exception if player names are not unique
  Returns new round object otherwise
   */
  def apply(gameState: Move, lastMovePlayedBy: String,
            currentPlayerTurn: Int, listOfPlayers: List[Player],
            roundPassStatus: Map[String, Boolean], movesPlayed: List[Move] = List.empty): Round = {
    if(listOfPlayers.size - listOfPlayers.map(p => p.name).distinct.size != 0) throw DuplicatePlayerNamesException("Player names must be unique")
    else new Round(gameState, lastMovePlayedBy, currentPlayerTurn, listOfPlayers, roundPassStatus, movesPlayed)
  }

}

case class DuplicatePlayerNamesException(s: String) extends IllegalStateException(s)

