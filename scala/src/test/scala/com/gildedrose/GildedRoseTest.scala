package com.gildedrose

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.gildedrose.GildedRose._

class GildedRoseTest  extends AnyWordSpec with Matchers {
      "Quality" should {
        "other item decrease quality  1 " in {
          val items = Array[Item](Item("foo", 0, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 0
        }

        "Aged Brie increase quality 1" in {
          val items = Array[Item](Item(agedBrie, 7, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 2
        }
        "Aged Brie increase quality 2" in {
          val items = Array[Item](Item(agedBrie, 0, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 3
          app.items(0).sellIn shouldBe -1
        }

        "Sulfuras no changes" in {
          val items = Array[Item](Item(sulfuras, 0, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 1
          app.items(0).sellIn shouldBe 0
        }
        "Backstage sellIn -1" in {
          val items = Array[Item](Item(backstage, 0, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 0
          app.items(0).sellIn shouldBe -1
        }

        "Backstage quality > 50 sellIn -1" in {
          val items = Array[Item](Item(backstage, 0, 51))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 0
          app.items(0).sellIn shouldBe -1
        }

        "Backstage quality > 50 sellIn 12" in {
          val items = Array[Item](Item(backstage, 12, 51))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 51
          app.items(0).sellIn shouldBe 11
        }

        "Backstage quality > 50 sellIn 2" in {
          val items = Array[Item](new Item(backstage, 2, 51))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 51
          app.items(0).sellIn shouldBe 1
        }

        "Backstage quality < 50 sellIn 2" in {
          val items = Array[Item](Item(backstage, 2, 49))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 50
          app.items(0).sellIn shouldBe 1
        }

        "Ragnaros quality < 0 " in {
          val items = Array[Item](Item(sulfuras, 0, -1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe -1
          app.items(0).sellIn shouldBe 0
        }
        "other item sellIn < 0 quality >0 " in {
          val items = Array[Item](Item("foo", -1, 2))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality shouldBe 0
        }

      }
      "Sell In" should {
        "decrease 1" in {
          val items = Array[Item](Item("foo", 1, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).sellIn shouldBe 0
        }
      }

     "GilderRose" should{
       "increaseQualityItem" in {
         val item = Item("foo", 1, 1)
         increaseQualityItem(item) shouldBe Item("foo", 1, 2)
       }
        "decreaseQualityItem" in {
          val item = Item("foo", 1, 1)
          decreaseQualityItem(item) shouldBe Item("foo", 1, 0)
        }
       "reduceTotalQuality" in {
         val item = Item("foo", 1, 10)
         reduceTotalQuality(item) shouldBe Item("foo", 1, 0)
       }
       "reduceTotalQuality negtive" in {
         val item = Item("foo", 1, -10)
         reduceTotalQuality(item) shouldBe Item("foo", 1, 0)
       }

       "decreaseSellInItem" in  {
         val item = Item("foo", 1, 10)
         decreaseSellInItem(item) shouldBe Item("foo", 0, 10)
       }

     }
}