package com.gildedrose

class GildedRose(val items: Array[Item]) {
import GildedRose._


  def updateQuality() {
    for (i <- 0 until items.length) {
      if (items(i).name != agedBrie
        && items(i).name != backstage) {
        if (items(i).quality > 0) {
          if (items(i).name != sulfuras) {
            items(i)= decreaseQualityItem(items(i))
          }
        }
      } else {
        if (items(i).quality < middleQuality) {
          items(i)= increaseQualityItem(items(i))
          increaseQualityBySellIn(i)
        }
      }

      if (items(i).name != sulfuras) {
        items(i)= decreaseSellInItem(items(i))
      }

      updateQualityByLowSellIn(i)
    }
  }

  private def updateQualityByLowSellIn(i: Int) = {
    if (items(i).sellIn < 0) {
      if (items(i).name != agedBrie) {
        if (items(i).name != backstage) {
          if ((items(i).quality > 0) && (items(i).name != sulfuras)) {
              items(i) = decreaseQualityItem(items(i))
          }
        } else {
          items(i) = reduceTotalQuality(items(i))
        }
      } else {
        if (items(i).quality < middleQuality) {
          items(i) = increaseQualityItem(items(i))
        }
      }
    }
  }

  private def increaseQualityBySellIn(i: Int) = {
    if (items(i).name == backstage && items(i).quality < middleQuality) {
      if (items(i).sellIn < lowSellIn) {
          items(i) = increaseQualityItem(items(i))
      }

      if (items(i).sellIn < veryLowSellIn) {
          items(i) = increaseQualityItem(items(i))
      }
    }
  }
}
object GildedRose{
  val agedBrie = "Aged Brie"
  val backstage =  "Backstage passes to a TAFKAL80ETC concert"
  val sulfuras = "Sulfuras, Hand of Ragnaros"

  val middleQuality = 50
  val lowSellIn = 11
  val veryLowSellIn = 6


  def increaseQualityItem(item: Item) =  Item(item.name, item.sellIn, item.quality + 1)
  def decreaseQualityItem(item: Item) =  Item(item.name, item.sellIn, item.quality - 1)
  def reduceTotalQuality(item: Item) = Item(item.name, item.sellIn, 0)
  def decreaseSellInItem(item: Item)= Item(item.name, item.sellIn -1 , item.quality)
}