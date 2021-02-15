package com.gildedrose

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GildedRoseTest  extends AnyWordSpec with Matchers {
      "Quality" should {
        "decrease 1 " in {
          val items = Array[Item](new Item("foo", 0, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 0
        }

        "increase 1" in {
          val items = Array[Item](new Item("Aged Brie", 7, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 2
        }
        "increase 2" in {
          val items = Array[Item](new Item("Aged Brie", 0, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 3
        }



      }
      "Sell In" should {
        "decrease 1" in {
          val items = Array[Item](new Item("foo", 1, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).sellIn shouldBe 0
        }
      }
}