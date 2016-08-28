import java.io._
import scala.xml.{Node, NodeSeq}
import com.typesafe.config.ConfigFactory

/**
  * Created by cloudera on 8/22/16.
  */

object XmlCsvShema extends App {
  case class Column (columnName: String, dataType: String, length: String, isNullable: String, constraints: Seq[(String,String)])

  val lp = LolSimpleParser

  val xml = <Tables>
          |  <Table Name="Farmatic_Compras" Schema="dbo">
          |    <columns>
          |      <Column Name="pharmacy_code" DataType="varchar" Length="10" IsNullable="NO" />
          |      <Column Name="EAN_articulo" DataType="varchar" Length="50" IsNullable="YES">
          |        <constraints>
          |         <Constraint Name="UNIQUE" VALUE="" />
          |        </constraints>
          |      </Column>
          |      <Column Name="Descripcion" DataType="varchar" Length="50" IsNullable="YES" />
          |      <Column Name="Unidades" DataType="int" IsNullable="YES">
          |        <constraints>
          |          <Constraint Name="CHECK" VALUE="([Unidades]&gt;(0))" />
          |          <Constraint Name="CHECK" VALUE="([Unidades]&lt;[Stock])" />
          |        </constraints>
          |      </Column>
          |    </columns>
          |  </Table>
          |
          |  <Table Name="ph2b" Schema="dbo">
          |    <columns>
          |      <Column Name="CANTIDAD" DataType="bigint" IsNullable="NO">
          |        <constraints>
          |          <Constraint Name="CHECK" VALUE="([CANTIDAD]&gt;(0))" />
          |        </constraints>
          |      </Column>
          |      <Column Name="CDG MEDICO" DataType="varchar" Length="20" IsNullable="NO" />
          |      <Column Name="FECHA" DataType="varchar" Length="20" IsNullable="NO">
          |        <!-- <constraints>
          |          <Constraint Name="CHECK" VALUE="([FECHA]&gt;='January 1, 1970' AND [FECHA]&lt;=getdate())" />
          |        </constraints> -->
          |      </Column>
          |      <Column Name="MED ESPEC" DataType="varchar" Length="50" IsNullable="NO" />
          |      <Column Name="TICKET" DataType="bigint" IsNullable="NO">
          |        <constraints>
          |          <Constraint Name="PRIMARY KEY" VALUE="" />
          |          <Constraint Name="CHECK" VALUE="([TICKET]&gt;(0))" />
          |        </constraints>
          |      </Column>
          |    </columns>
          |  </Table>
          |</Tables>

  val dictionary = ConfigFactory.load ("dictionary")


  def createConstraints (constraints: Seq[(String,String)]) ={
//TODO FECHA CHECK IS NOT WORKING DUE SPACES
    // REAL CONVERSION TO CSVS NOW THE OUTPUT IS:
    /*
    ([Unidades]>(0)) ---> ([unidades] greater than (0))
([Unidades]<[Stock]) ---> ([unidades] less than [stock])
([CANTIDAD]>(0)) ---> ([cantidad] greater than (0))
([TICKET]>(0)) ---> ([ticket] greater than (0))
     */
    // AND SOULD BE:

    //List() OK
    //List((UNIQUE,)) OK
    //List((CHECK,([Unidades]>(0))), (CHECK,([Unidades]<[Stock]))) --->  range(0, *) // not allowed -> range (*, $Stock)
    //List((CHECK,([CANTIDAD]>(0)))) range (0, *)
    //List((CHECK,([FECHA]>='January 1, 1970' AND [FECHA]<=getdate()))) ---> xDateTime(1970-01-01 , scala getDate  :-( 2015-12-03)
    //List((PRIMARY KEY,), (CHECK,([TICKET]>(0)))) OK ---> range (0, *)

    // SO A CHANGE IN LolSimple Parser is needed and the name of the columns should be passed

    def createCheck (check: String) = {

      val constraint = lp.getCheckConstraints (check)
      println (check + " ---> " + constraint )
      constraint
    }

    val constraintsDot = "constraints."
    constraints.map {p => p match {
      case p if (p._1.equals("UNIQUE")) => dictionary.getString (s"${constraintsDot}unique")
      case p if (p._1.equals("PRIMARY KEY")) => dictionary.getString (s"${constraintsDot}unique")
      case p if (p._1.equals("CHECK")) => createCheck (p._2)
      case _ => ""
    }}.mkString(" ")

  }

  def createRuleFromColumn (column: Column) ={
    val dataTypeDot = "dataType."
    val dataType = dictionary.getString (s"${dataTypeDot}${column.dataType}")

    val length = if (column.length.isEmpty) "" else s"length(*, ${column.length})"

    val notEmpty = column.isNullable match {
      case "YES" => ""
      case "NO" => "notEmpty"
      case _ => ""
    }

    val constraints = createConstraints (column.constraints)

    s"${column.columnName}:${dataType} ${length} ${notEmpty} ${constraints}"
  }

  def createRuleFromXMLColumn (column: Node) = {

    def getColumnAttribute (attribute: String ) = {
      val colName = column.attribute(attribute)
      colName match {
        case Some(x) => {
          x.toList(0).toString
        }
        case None => ""
      }
    }
    def getNameValue (pair: (NodeSeq, NodeSeq)) = {
      val keyNodes = pair._1
      val valueNodes = pair._2

      val key = if (keyNodes.length > 0) keyNodes(0).text else ""
      val value = if (valueNodes.length > 0) valueNodes(0).text else ""

      (key,value)
    }

    def getConstraints () = {
      val constraints = column \ "constraints" \ "Constraint"
      if (!constraints.isEmpty) {
        val pairNodeConstraints = constraints.map(node => (node \ "@Name", node \ "@VALUE"))
        pairNodeConstraints.map (p => getNameValue (p))
      } else List ()
    }

    val columnName = getColumnAttribute ("Name")
    val dataType = getColumnAttribute ("DataType")
    val length = getColumnAttribute ("Length")
    val isNullable = getColumnAttribute("IsNullable")
    val constraints = getConstraints ()


    if (columnName.isEmpty) "" else createRuleFromColumn (Column (columnName, dataType, length, isNullable, constraints))
  }

  def csvSchemaHeader(columns: NodeSeq): String = {
    s"""
      |version 1.0
      |@separator '${dictionary.getString("globalDirectives.separator")}'
      |@totalColumns ${columns.length}
    """.stripMargin.trim
  }

  def createSchemaColumnRules (columns: NodeSeq) =
    columns.map (columnNode => createRuleFromXMLColumn (columnNode)).filter (!_.isEmpty).mkString("\n")

  def createCsvSchema (table: Node) ={
    table.attribute("Name") match {
      case Some (x) => {
        val tableName = x.toList(0)
        val writer = new FileWriter(new File(tableName.toString))

        val columns =table \ "columns" \ "Column"

        val header = csvSchemaHeader(columns)

        val columnRules = createSchemaColumnRules (columns)

        writer.write(s"${header}\n\n${columnRules}")
        writer.close()
      }
      case None  => //nothing to do
    }
  }

  (xml \ "Table").foreach(createCsvSchema)

}
