package sw

import sw.TournamentTree.DefinedPlayer.{PrefilledPlayer, Win}
import sw.TournamentTree.{DefinedPlayer, Remaining}

object TournamentTree {

  def applyAny[T](players: T*): TournamentTree[T] = {
    players match {
      case Seq(one) => PrefilledPlayer(one)
      case other =>
        val totalSize = other.size
        val splitAt = java.lang.Integer.highestOneBit(totalSize) match {
          case `totalSize` => totalSize / 2
          case otherSize => otherSize
        }
        other.splitAt(splitAt) match {
          case (left, right) => Remaining(applyAny(left: _*), applyAny(right: _*))
        }
    }
  }

  def applyPowerOfTwo[T](players: T*): TournamentTree[T] = {
    if (players.length == 1) PrefilledPlayer(players(0))
    else {
      require(players.length % 2 == 0)
      players.splitAt(players.length / 2) match {
        case (left, right) =>
          Remaining(applyPowerOfTwo(left: _*), applyPowerOfTwo(right: _*))
      }
    }
  }

  sealed trait DefinedPlayer[T] extends TournamentTree[T] {
    def player: T
  }

  object DefinedPlayer {
    def unapply[T](input: DefinedPlayer[T]): Option[T] = PartialFunction.condOpt(input) {
      case PrefilledPlayer(player) => player
      case Win(player, _) => player
    }

    case class PrefilledPlayer[T](player: T) extends DefinedPlayer[T]

    case class Win[T](player: T, from: Remaining[T]) extends DefinedPlayer[T] {
      def left = from.left

      def right = from.right
    }

  }

  case class Remaining[T](left: TournamentTree[T], right: TournamentTree[T]) extends TournamentTree[T]

}

sealed trait TournamentTree[T] {

  def win(player: T): TournamentTree[T] = this match {
    case r@Remaining(DefinedPlayer(`player`), DefinedPlayer(_)) => Win(player, r)
    case r@Remaining(DefinedPlayer(_), DefinedPlayer(`player`)) => Win(player, r)
    case r@Remaining(left, right) => Remaining(left.win(player), right.win(player))
    case _ => this
  }

  def nextGames: List[Remaining[T]] = this match {
    case r@Remaining(DefinedPlayer(_), DefinedPlayer(_)) => List(r)
    case r@Remaining(a, b) => a.nextGames ++ b.nextGames
    case Win(_, _) => List.empty
    case PrefilledPlayer(_) => List.empty
  }

}

object DemoTournamentApp extends App {

  //  val tourney = TourneyBranch.applyPowerOfTwo("drakas", "lucas", "sanzo", "million")
  //  val tourney = TourneyBranch.applyAny("drakas", "lucas", "sanzo", "million")
  val tourney = TournamentTree.applyAny("drakas", "lucas", "sanzo")
  val result = tourney.win("drakas").win("sanzo").win("sanzo")
  println(result)
  println(result.nextGames)
}
