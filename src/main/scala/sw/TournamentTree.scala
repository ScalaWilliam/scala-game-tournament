package sw

import scala.annotation.tailrec

sealed trait TournamentTree[T] {

  def win(player: T): TournamentTree[T]

  def nextGames: Set[(T, T)]

  def winner: Option[T]

}

object TournamentTree {

  def apply[T](players: T*): TournamentTree[T] = {
    buildTree(players.map(DefinedPlayer.apply): _*)
  }

  case class DefinedPlayer[T](player: T) extends TournamentTree[T] {
    def win(player: T): TournamentTree[T] = this

    def nextGames: Set[(T, T)] = Set.empty

    def winner: Option[T] = Some(player)
  }

  case class UndefinedPlayer[T](left: TournamentTree[T], right: TournamentTree[T]) extends TournamentTree[T] {
    def win(player: T): TournamentTree[T] = (left, right) match {
      case (DefinedPlayer(`player`), DefinedPlayer(_)) => DefinedPlayer(player)
      case (DefinedPlayer(_), DefinedPlayer(`player`)) => DefinedPlayer(player)
      case _ => UndefinedPlayer(left.win(player), right.win(player))
    }

    def nextGames: Set[(T, T)] = (left, right) match {
      case (DefinedPlayer(a), DefinedPlayer(b)) => Set(a -> b)
      case _ => left.nextGames ++ right.nextGames
    }

    def winner: Option[T] = None
  }

  /** Bottom up build: take every 2 consecutive items, create an UndefinedPlayer,
    * then reprocess the new list of trees until we're left with only 1.
    */
  @tailrec
  private def buildTree[T](tournamentTree: TournamentTree[T]*): TournamentTree[T] = {
    tournamentTree.toList match {
      case tree :: Nil => tree
      case other =>
        buildTree {
          other.grouped(2).map(_.toList).map {
            case a :: b :: Nil => UndefinedPlayer(a, b)
            case a :: _ => a
            case Nil => ???
          }.toList: _*
        }
    }
  }

}
