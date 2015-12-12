

class Purse(init: Int) {
  var money: Int = init
  def add_money(m: Int) {
    money += m
  }
  def take_money(m: Int) {
    money -= m
  }
}