package sw

import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * Created by me on 14/12/2016.
  */
class TournamentTreeSpec extends FunSuite {
  val Drakas = "Drakas"
  val Lucas = "Lucas"
  val Sanzo = "Sanzo"
  private val tourney = TournamentTree(Drakas, Lucas, Sanzo)

  test("First iteration is correct") {
    tourney.nextGames should contain theSameElementsAs Set(Drakas -> Lucas)
    tourney.winner shouldBe empty
  }
  test("Sanzo win does nothing") {
    tourney.win(Sanzo).nextGames shouldEqual tourney.nextGames
    tourney.win(Sanzo).winner shouldEqual tourney.winner
  }
  test("Drakas win pushes to next stage") {
    tourney.win(Drakas).nextGames should contain theSameElementsAs Set(Drakas -> Sanzo)
    tourney.win(Drakas).winner shouldBe empty
  }
  test("Drakas win 2x pushes to final stage") {
    tourney.win(Drakas).win(Drakas).nextGames shouldBe empty
    tourney.win(Drakas).win(Drakas).winner shouldBe Some(Drakas)
  }

}
