package models

import java.io._

import scala.io.Source
import scala.xml._
import scala.collection.JavaConversions._
import scala.math._

import play._
import play.cache._
import play.libs.WS

import org.apache.lucene.util.Version
import org.apache.lucene.analysis.snowball.SnowballAnalyzer
import org.apache.lucene.analysis._
import org.apache.lucene.analysis.tokenattributes._

class Tweet(val id: Long, val text: String){
 override def toString = id + " => " + text
}

object Tweet{
  lazy val url = Play.configuration.getProperty("twitbrother.api.status.url")
  lazy val frequencies: Map[String, Double] = loadFrequencies
  
  def apply(id: Long, text: String) = new Tweet(id, text)
  def unapply(t: Tweet) = Some((t.id, t.text))
  
  def getForUser(user: String) = Cache.get(user, "30mn"){
    val fullURL = url.format(user)
    val tweets = XML.load(WS.url(fullURL).get().getStream) \ "status"
    tweets map (t => {
      val text = (t \ "text").text
      val id   = (t \ "id").text.toLong
      Tweet(id, text)
    })
  }
  
  def getTags(tweets: Seq[Tweet]) = {
    
    val acc = ("" /: tweets){ _ + _.text }
    
    val stopwords: List[String] = StopAnalyzer.ENGLISH_STOP_WORDS_SET.toList.asInstanceOf[List[String]] ::: List("http", "rt") ::: ((0 to 10).toList map(_.toString))
    
    
    //TODO add RT, http, etc to stopwrods    
    val analyzer = new SnowballAnalyzer(Version.LUCENE_30,"English", stopwords.toArray)
    val tokenStream = analyzer.tokenStream("contents", new StringReader(acc))
    val termAtt = tokenStream.getAttribute(classOf[TermAttribute]).asInstanceOf[TermAttribute]
    
    var sum = 0
    def countRep(tokenStream: TokenStream, occ:  Map[String, Int]): Map[String, Int] = {
       if(tokenStream.incrementToken){
          sum += 1
          val term = termAtt.term
          countRep(tokenStream, if(occ contains term) occ updated(term, occ(term) + 1) else occ updated(term, 1))
        }
        else
          occ
    }
    
    val count = countRep(tokenStream, Map[String, Int]())
    val l = count.toList
    val tfs = l map { x => (x._1, x._2.toDouble / sum) }
    tfs map { x => 
      ( x._1, x._2 * frequencies.getOrElse(x._1, 50d).asInstanceOf[Double] )
    } sortWith( _._2 > _._2 )
  }
  
  private def loadFrequencies() = {
    val path = Play.applicationPath.getAbsolutePath + "/data/lemma.num"
    var total = 0
    val r = ( Source.fromFile(path).getLines map { x: String =>
      val l = x.split(" ")
      var f =  l(1).toInt
      total += f
      (l(2) -> f)
    } toMap )

    r mapValues{x: Int => log( total.toDouble / x )}
  }
  
}