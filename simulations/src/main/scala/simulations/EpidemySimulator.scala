package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt
  def randomPick[T](i: List[T]) = i(randomBelow(i.size))

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val initialRate = 100
    val sickInterval = 6
    val deathInterval = 8
    val deathChance = 25
    val immuneInterval = 20
    val healthInterval = 2
    val infectionRate = 40

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = (1 to 300).map { id =>
    val p = new Person(id)
    p.move()
    if (id % initialRate == 0)
      p.becomeInfected()
    p
  }.toList

  def personsInRoom(room: (Int, Int)) =
    persons.filter(p => p.row == room._1 && p.col == room._2)

  def neighbourRoom(person: Person) =
    for {
      x <- List(-1, 1)
      y <- List(-1, 1)
    } yield ((person.row + x + 8) % 8, (person.col + y + 8) % 8)

  def hasAnyVisiblyInfected(room: (Int, Int)) =
    personsInRoom(room).filter(isVisiblyInfected).size > 0

  def isVisiblyInfected(person: Person) =
    person.sick || person.dead

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def move() {
      afterDelay(randomBelow(4) + 1) {
        if (!dead) {
          val possibilities = neighbourRoom(this).filterNot(hasAnyVisiblyInfected)
          if (possibilities.size > 0) {
            val (new_row, new_col) = randomPick(possibilities)
            val gets_infected = personsInRoom((new_row, new_col)).exists(_.infected) && (randomBelow(100) < infectionRate)
            val willGetInfected = (!immune && gets_infected)

            afterDelay(0) {
              walkIntoRoom(new_row, new_col)
              if (!infected && willGetInfected) {
                becomeInfected()
              }
              move()
            }
          }
        }
      }
    }

    def becomeInfected() {
      afterDelay(sickInterval) {
        becomeSick()
      }
      infected = true
    }

    def becomeSick() {
      afterDelay(deathInterval) {
        if (randomBelow(100) < deathChance)
          dead = true
        else {
          afterDelay(immuneInterval) {
            becomeImmune()
          }
        }
      }
      sick = true
    }

    def becomeImmune() {
      afterDelay(healthInterval) {
        becomeHealthy()
      }
      immune = true
    }

    def becomeHealthy() {
      infected = false
      sick = false
      immune = false
      dead = false
    }

    private def walkIntoRoom(new_row: Int, new_col: Int) = {
      row = new_row
      col = new_col
    }
  }
}
