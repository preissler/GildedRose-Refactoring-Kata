package com.gildedrose

class GildedRose(val items: Array[Item]) {



  def increaseQualityItem(item: Item) =  Item(item.name, item.sellIn, item.quality + 1)
  def decreaseQualityItem(item: Item) =  Item(item.name, item.sellIn, item.quality - 1)
  def reduceTotalQuality(item: Item) = Item(item.name, item.sellIn, item.quality - item.quality)
  def decreaseSellInItem(item: Item)= Item(item.name, item.sellIn -1 , item.quality)

  def updateQuality() {
    for (i <- 0 until items.length) {
      if (!items(i).name.equals("Aged Brie")
        && !items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
        if (items(i).quality > 0) {
          if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
            items(i)= decreaseQualityItem(items(i))
          }
        }
      } else {
        if (items(i).quality < 50) {
          items(i)= increaseQualityItem(items(i))

          if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
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

      if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
        items(i)= decreaseSellInItem(items(i))
      }

      if (items(i).sellIn < 0) {
        if (!items(i).name.equals("Aged Brie")) {
          if (!items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (items(i).quality > 0) {
              if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
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