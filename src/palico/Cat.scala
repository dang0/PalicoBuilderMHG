package palico

import scala.collection.mutable.ListBuffer
import java.awt.Color
import scala.swing.Publisher
import scala.reflect.runtime.universe.{TypeTag, typeOf}

object Cat extends Publisher {
  var catClass: CatClass = Charisma
  def defaultMove(): SupportMoves = catClass.defaultMove
  def secondaryMoves(): List[SupportMoves] = catClass.secondaryMoves
  def defaultSkills(): List[Skills] = catClass.defaultSkills
  def fullMoveList(): List[SupportMoves] = defaultMoveList ++ moveListBuffer
  val defaultMoveList: List[SupportMoves] =  List(Mini_Barrel_Bombay, Herb_Horn)
  val moveListBuffer: ListBuffer[SupportMoves] = ListBuffer(Shock_Purrison,Shock_Tripper)
  val skillListBuffer: ListBuffer[Skills] = ListBuffer()
  
  final def MAX_SUPPORT_MOVES: Int = 12
  final def MAX_SUPPORT_COST: Int = 9
  final def MAX_SKILL_COST: Int = 8
  
  def getList[T <: PBListItem](implicit tag: TypeTag[T]): List[T] = tag match {
    case moves if typeOf[T] == typeOf[SupportMoves] => moveListBuffer.toList.asInstanceOf[List[T]]
    case skills if typeOf[T] == typeOf[Skills] => skillListBuffer.toList.asInstanceOf[List[T]]
    case _ => throw new Exception("")
  }
  
  def getMaxCost[T <: PBListItem](implicit tag: TypeTag[T]): Int = tag match {
    case moves if typeOf[T] == typeOf[SupportMoves] => MAX_SUPPORT_COST
    case skills if typeOf[T] == typeOf[Skills] => MAX_SKILL_COST
    case _ => -1
  }
  
  def availablePoints[T <: PBListItem](implicit tag: TypeTag[T]): Int = {
    var total = 0
    println(getList[T])
    getList[T].foreach { total += _.cost }
    getMaxCost[T] - total
  }
}

trait CatClass {
  var classColor: Color = Color.BLACK
  var defaultMove: SupportMoves = null
  var secondaryMoves: List[SupportMoves] = null
  var defaultSkills: List[Skills] = null
  
}
case object Charisma extends CatClass {
  classColor = Color.YELLOW
  defaultMove = Palico_Rally
  secondaryMoves = List(null)
  defaultSkills = List(Slacker_Slap, Last_Stand)
}
case object Fighting extends CatClass {
  classColor = Color.RED
  defaultMove = Furr_ious
  secondaryMoves = List(Demon_Horn, Piercing_Boomerang)
  defaultSkills = List(Attack_Up_S, Handicraft)
}
case object Protection extends CatClass {
  classColor = Color.BLUE
  defaultMove = Taunt
  secondaryMoves = List(Emergency_Retreat, Armor_Horn)
  defaultSkills = List(Guard_S, Guard_Boost)
}
case object Assisting extends CatClass {
  classColor = Color.MAGENTA
  defaultMove = Poison_Purr_ison
  secondaryMoves = List(Cheer_Horn, Emergency_Retreat)
  defaultSkills = List(Monsteradar, Pro_Trapper)
}
case object Healing extends CatClass {
  classColor = Color.GREEN
  defaultMove = True_Health_Horn
  secondaryMoves = List(Armor_Horn, Cheer_Horn)
  defaultSkills = List(Defense_Up_S, Health_Harmonics)
}
case object Bombing extends CatClass {
  classColor = Color.ORANGE
  defaultMove = Mega_Barrel_Bombay
  secondaryMoves = List(Camouflage, Demon_Horn)
  defaultSkills = List(Heat_Bomb_Res, Bombay_Boost)
}
case object Gathering extends CatClass {
  classColor = Color.WHITE
  defaultMove = Plunderang
  secondaryMoves = List(Piercing_Boomerang, Camouflage)
  defaultSkills = List(Gathering_Pro, Pilfer_Boost)
}

abstract class PBListItem(list: Double*) {
  var cost: Int = 0
  var desc: String = ""
  var bhernaRate, kokotoRate, pokkeRate, yukumoRate: Double = 1.0
  if(list.length > 3) {
    bhernaRate = list(0) / 100.0
    kokotoRate = list(1) / 100.0
    pokkeRate = list(2) / 100.0
    yukumoRate = list(3) / 100.0
  }
}

object SupportMoves {
  final val allMoves: List[SupportMoves] = List(
      Palico_Rally, Furr_ious, Taunt, Poison_Purr_ison, True_Health_Horn, Mega_Barrel_Bombay, Plunderang,
      Mini_Barrel_Bombay, Herb_Horn, Demon_Horn, Piercing_Boomerang, Emergency_Retreat, Armor_Horn, Cheer_Horn,
      Camouflage, Health_Horn, Anti_Monster_Mine_Plus, Pilfer, Pitfall_Purrison, Shock_Purrison, Giga_Barrel_Bombay, Rath_of_Meow,
      Claw_Dance, Weapon_Upgrade, Trampoliner, Go_Fight_Win, Detox_Horn, Vase_of_Vitality, Mega_Boomerang, Flash_Bombay,
      Big_Barrel_Bombay, Anti_Monster_Mine, Sumo_Stomp, Felyne_Comet,  Dung_Bombay,  Ultrasonic_Horn,  Soothing_Roll,  Parting_Gift,
      Excavator, Big_Boomerang, Shock_Tripper, Chestnut_Cannon, Barrel_Bombay,  Bounce_Bombay,  Explosive_Roll )
}

abstract class SupportMoves(list: Double*) extends PBListItem(list: _*) {
  var moveType: String = ""
  
}
trait DefaultMoves extends SupportMoves {}
case object Palico_Rally extends DefaultMoves
case object Furr_ious extends DefaultMoves
case object Taunt extends DefaultMoves
case object Poison_Purr_ison extends DefaultMoves
case object True_Health_Horn extends DefaultMoves
case object Mega_Barrel_Bombay extends DefaultMoves
case object Plunderang extends DefaultMoves
case object Mini_Barrel_Bombay extends DefaultMoves
case object Herb_Horn extends DefaultMoves

trait SecondaryMoves extends SupportMoves {
  cost = 1
}
case object Demon_Horn extends SecondaryMoves
case object Piercing_Boomerang extends SecondaryMoves
case object Emergency_Retreat extends SecondaryMoves
case object Armor_Horn extends SecondaryMoves
case object Cheer_Horn extends SecondaryMoves
case object Camouflage extends SecondaryMoves

abstract class MoveGroupA(list: Double*) extends SupportMoves(list: _*) {
  cost = 3
}
case object Health_Horn extends MoveGroupA(15,1,5,15)
case object Anti_Monster_Mine_Plus extends MoveGroupA(15,25,5,5)
case object Pilfer extends MoveGroupA(10,5,5,20)
case object Pitfall_Purrison extends MoveGroupA(15,5,15,25)
case object Shock_Purrison extends MoveGroupA(15,5,15,25)
case object Giga_Barrel_Bombay extends MoveGroupA(15,25,5,5)
case object Rath_of_Meow extends MoveGroupA(15,25,5,5)

abstract class MoveGroupB(list: Double*) extends SupportMoves(list: _*) {
  cost = 2
}
case object Claw_Dance extends MoveGroupB(10,18,3,3)
case object Weapon_Upgrade extends MoveGroupB(10,13,3,3)
case object Trampoliner extends MoveGroupB(10,4,10,25)
case object Go_Fight_Win extends MoveGroupB(10,4,10,25)
case object Detox_Horn extends MoveGroupB(10,4,30,10)
case object Vase_of_Vitality extends MoveGroupB(10,4,30,10)
case object Mega_Boomerang extends MoveGroupB(10,13,3,3)
case object Flash_Bombay extends MoveGroupB(10,4,5,15)
case object Big_Barrel_Bombay extends MoveGroupB(10,18,3,3)
case object Anti_Monster_Mine extends MoveGroupB(10,18,3,3)

abstract class MoveGroupC(list: Double*) extends SupportMoves(list: _*) {
  cost = 1
}
case object Sumo_Stomp extends MoveGroupC(8,10,4,4)
case object Felyne_Comet extends MoveGroupC(8,10,4,4)
case object Dung_Bombay extends MoveGroupC(7,4,15,20)
case object Ultrasonic_Horn extends MoveGroupC(8,4,10,25)
case object Soothing_Roll extends MoveGroupC(8,4,15,4)
case object Parting_Gift extends MoveGroupC(8,4,24,4)
case object Excavator extends MoveGroupC(7,10,4,4)
case object Big_Boomerang extends MoveGroupC(8,10,4,4)
case object Shock_Tripper extends MoveGroupC(8,4,4,15)
case object Chestnut_Cannon extends MoveGroupC(8,10,4,4)
case object Barrel_Bombay extends MoveGroupC(8,10,4,4)
case object Bounce_Bombay extends MoveGroupC(7,10,4,4)
case object Explosive_Roll extends MoveGroupC(7,10,4,4)

abstract class Skills(list: Double*) extends PBListItem(list: _*) {
  var slots: Int = 0
  var color: Color = Color.BLACK 
}
//default skills
case object Slacker_Slap extends Skills
case object Last_Stand extends Skills
case object Attack_Up_S extends Skills
case object Handicraft extends Skills
case object Guard_S extends Skills
case object Guard_Boost extends Skills
case object Monsteradar extends Skills
case object Pro_Trapper extends Skills
case object Defense_Up_S extends Skills
case object Health_Harmonics extends Skills
case object Heat_Bomb_Res extends Skills
case object Bombay_Boost extends Skills
case object Gathering_Pro extends Skills
case object Pilfer_Boost extends Skills

abstract class SkillGroupA(list: Double*) extends Skills(list: _*) {
  cost = 3
}
case object Element_Attack_Up extends SkillGroupA(15,25,5,5)
case object Status_Attack_Up extends SkillGroupA(10,25,5,10)
case object Anger_Prone extends SkillGroupA(15,25,5,5)
case object Revival_Pro extends SkillGroupA(15,5,30,15)
case object Omniresistance extends SkillGroupA(15,5,25,5)
case object Support_Priority extends SkillGroupA(15,5,15,35)
case object Support_Move_1 extends SkillGroupA(15,10,15,25)

abstract class SkillGroupB(list: Double*) extends Skills(list: _*) {
  cost = 2
}
case object Attack_Up_L extends SkillGroupB(10,20,5,5)
case object Critical_Up_L extends SkillGroupB(8,15,5,5)
case object Defense_Up_L extends SkillGroupB(10,5,25,5)
case object Health_Up_L extends SkillGroupB(10,5,20,5)
case object Nine_Lives_Attack extends SkillGroupB(8,15,5,5)
case object Guard_L extends SkillGroupB(10,5,5,20)
case object Knockout_King extends SkillGroupB(8,5,5,20)
case object Earplugs extends SkillGroupB(8,5,10,5)
case object Negate_Stun extends SkillGroupB(8,5,15,5)
case object Counter_Boost extends SkillGroupB(10,15,5,5)
case object Support_Boost extends SkillGroupB(10,5,5,20)

abstract class SkillGroupC(list: Double*) extends Skills(list: _*) {
  cost = 1
}
case object Critical_Up_S extends SkillGroupC(6,20,3,4)
case object Health_Up_S extends SkillGroupC(8,6,8,16)
case object Nine_Lives_Defense extends SkillGroupC(6,4,8,10)
case object Boomerang_Pro extends SkillGroupC(6,15,3,4)
case object Stamina_Drain extends SkillGroupC(6,15,3,10)
case object Non_Stick_Fur extends SkillGroupC(6,4,8,15)
case object Negate_Wind extends SkillGroupC(8,4,8,4)
case object Negate_Sleep extends SkillGroupC(6,4,8,4)
case object Iron_Hide extends SkillGroupC(6,4,8,4)
case object Negate_Paralysis extends SkillGroupC(6,4,8,4)
case object Tremor_Res extends SkillGroupC(6,4,8,4)
case object Negate_Poison extends SkillGroupC(8,4,8,4)
case object Biology extends SkillGroupC(6,4,8,4)
case object Negate_Confusion extends SkillGroupC(8,4,8,4)
case object Goldenfish_Catcher extends SkillGroupC(8,4,3,10)
