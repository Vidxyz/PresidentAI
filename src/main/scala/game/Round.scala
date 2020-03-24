package game

import player.Player



case class Round(gameState: Move,
                 lastMovePlayedBy: String,
                 totalNumberOfPlayers: Int,
                 currentPlayerTurn: Int,
                 listOfPlayers: List[Player],
                 roundPassStatus: List[Boolean]) {

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

  def playerEndedTheGameOnABurn: Boolean =
    !this.listOfPlayers.map(player => player.name).contains(this.lastMovePlayedBy) && this.lastMovePlayedBy != ""

  /*
  Since name is unique, this should only return a list of size 1
   */
  def hasAlreadySkippedTurn(name: String): Boolean = {
    (listOfPlayers zip roundPassStatus)
      .filter(tuple => tuple match {
        case (player, _passStatus) => player.name == name
      })
      .map {
        case (_, status) =>
          status
      }
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

}

object Round {
  def getNoPassList(numberOfPlayers: Int): List[Boolean] = {
    (1 to numberOfPlayers).toList.map(_ => false)
  }

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

