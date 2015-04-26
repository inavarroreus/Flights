package com.stratio.model

import com.stratio.utils.ParserUtils
import org.joda.time.DateTime

sealed case class Cancelled (id: String) {override def toString: String = id}

object OnTime extends Cancelled (id ="OnTime")
object Cancel extends Cancelled (id ="Cancel")
object Unknown extends Cancelled (id ="Unknown")

case class Delays (
    carrier: Cancelled,
    weather: Cancelled,
    nAS: Cancelled,
    security: Cancelled,
    lateAircraft: Cancelled)

case class Flight (date: DateTime, //Tip: Use ParserUtils.getDateTime
    departureTime: Int,
    crsDepatureTime: Int,
    arrTime: Int,
    cRSArrTime: Int,
    uniqueCarrier: String,
    flightNum: Int,
    actualElapsedTime: Int,
    cRSElapsedTime: Int,
    arrDelay: Int,
    depDelay: Int,
    origin: String,
    dest: String,
    distance: Int,
    cancelled: Cancelled,
    cancellationCode: Int,
    delay: Delays)
{
  def isGhost: Boolean = arrTime == -1

  def departureDate: DateTime =
    date.hourOfDay.setCopy(departureTime.toString.substring(0, departureTime.toString.size - 2)).minuteOfHour
      .setCopy(departureTime.toString.substring(departureTime.toString.size - 2)).secondOfMinute.setCopy(0)

  def arriveDate: DateTime =
    date.hourOfDay.setCopy(departureTime.toString.substring(0, departureTime.toString.size - 2)).minuteOfHour
      .setCopy(departureTime.toString.substring(departureTime.toString.size - 2)).secondOfMinute.setCopy(0)
      .plusMinutes(cRSElapsedTime)
}

object Flight{

  /*
  *
  * Create a new Flight Class from a CSV file
  *
  */
  //"1987,13,14,3,741,730,912,849,PS,1451,NA,91,79,NA,23,12,SAN,SFO,447,NA,NA,0,NA,0,NA,NA,NA,NA,NA"
  def apply(fields: Array[String]): Flight = Flight(
    ParserUtils.getDateTime( fields(0).toInt, fields(1).toInt, fields(2).toInt),
    fields(4).toInt,
    fields(5).toInt,
    fields(6).toInt,
    fields(7).toInt,
    fields(8),
    fields(9).toInt,
    fields(11).toInt,
    fields(12).toInt,
    fields(14).toInt,
    fields(15).toInt,
    fields(16),
    fields(17),
    fields(18).toInt,
    parseCancelled(fields(22)),
    0, //TODO
    Delays( parseCancelled(fields(24)),
            parseCancelled(fields(25)),
            parseCancelled(fields(26)),
            parseCancelled(fields(27)),
            parseCancelled(fields(28)))
  )

  /*
   *
   * Extract the different types of errors in a string list
   *
   */
  def extractErrors(fields: Array[String]): Seq[String] = Seq(
    parsers.isInt(fields(0)),
    parsers.isMonthNumber(fields(1)),
    parsers.isDayNumber(fields(2)),
    parsers.isInt(fields(4)),
    parsers.isInt(fields(5)),
    parsers.isInt(fields(6)),
    parsers.isInt(fields(7)),
    parsers.isInt(fields(9)),
    parsers.isInt(fields(11)),
    parsers.isInt(fields(12)),
    parsers.isInt(fields(14)),
    parsers.isInt(fields(15)),
    parsers.isInt(fields(18))
  ).filter( _ != "")

  /*
  *
  * Parse String to Cancelled Enum:
  *   if field == 1 -> Cancel
  *   if field == 0 -> OnTime
  *   if field <> 0 && field<>1 -> Unknown
  */
  def parseCancelled(field: String): Cancelled = field match {
    case "1" => Cancel
    case "0" => OnTime
    case _ => Unknown
  }
}

object parsers {
  def isInt(s: String): String = try {
    s.toInt
    ""
  } catch {
    case _: java.lang.NumberFormatException => "NAN"
  }

  def isMonthNumber(s: String): String = try {
    if(s.toInt <= 12 && s.toInt > 0)
      ""
    else
      "Invalid date"
  }catch{
    case _ : java.lang.NumberFormatException => "NAN"
  }

  def isDayNumber(s : String): String = try {
    if(s.toInt <= 31 && s.toInt > 0)
      ""
    else
      "Invad date"
  }catch {
    case _ : java.lang.NumberFormatException => "NAN"
  }
}












