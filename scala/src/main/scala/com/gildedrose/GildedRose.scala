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
        if (items(i).quality < 50) {
          items(i)= increaseQualityItem(items(i))

          if (items(i).name == backstage) {
            if (items(i).sellIn < 11) {
              if (items(i).quality < 50) {
                items(i)= increaseQualityItem(items(i))
              }
            }

            if (items(i).sellIn < 6) {
              if (items(i).quality < 50) {
                items(i)= increaseQualityItem(items(i))
              }
            }
          }
        }
      }

      if (items(i).name != sulfuras) {
        items(i)= decreaseSellInItem(items(i))
      }

      if (items(i).sellIn < 0) {
        if (items(i).name != agedBrie) {
          if (items(i).name != backstage) {
            if (items(i).quality > 0) {
              if (items(i).name != sulfuras) {
                items(i)= decreaseQualityItem(items(i))
              }
            }
          } else {
            items(i)= reduceTotalQuality( items(i))
          }
        } else {
          if (items(i).quality < 50) {
            items(i)= increaseQualityItem(items(i))
          }
        }
      }
    }
  }
}
object GildedRose{
  val agedBrie = "Aged Brie"
  val backstage =  "Backstage passes to a TAFKAL80ETC concert"
  val sulfuras = "Sulfuras, Hand of Ragnaros"


  def increaseQualityItem(item: Item) =  Item(item.name, item.sellIn, item.quality + 1)
  def decreaseQualityItem(item: Item) =  Item(item.name, item.sellIn, item.quality - 1)
  def reduceTotalQuality(item: Item) = Item(item.name, item.sellIn, 0)
  def decreaseSellInItem(item: Item)= Item(item.name, item.sellIn -1 , item.quality)
}