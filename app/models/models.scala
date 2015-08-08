package models

import java.util.{Date}
import java.util.{Calendar}
import java.time._
import java.util.concurrent.TimeUnit


import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import play.api.libs.json._
import play.api.Logger
import play.api.libs.functional.syntax._

import scala.collection.mutable.ListBuffer

case class Resource(id: Int, firstName: String, lastName: String, position: String, location:String)

object Resource{

  /**
   * Parse a Resource from a ResultSet
   */
  val resourceParser = {
    get[Int]("RESOURCES.ID") ~
    get[String]("RESOURCES.FIRST_NAME") ~
    get[String]("RESOURCES.LAST_NAME") ~
    get[String]("RESOURCES.POSITION") ~
    get[String]("RESOURCES.LOCATION") map {
      case id~firstName~lastName~position~location => Resource(id, firstName, lastName, position, location)
    }
  }

  /**
   * Retrieve a Resource from the id.
   */
  def findById(id: Integer): Option[Resource] = {
    DB.withConnection { implicit connection =>
      SQL("select * from RESOURCES where ID = {id}").on('id -> id).as(resourceParser.singleOpt)
    }
  }

  /**
   * Retrieve all resources
   *
   */
  def listAll: List[Resource]={
   	DB.withConnection { implicit connection =>
      SQL("select * from RESOURCES").as(resourceParser *)
    }
  }

  /**
   * Update a resource.
   *
   * @param resource The resource values.
   */
  def update(resource: Resource) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update RESOURCES
          set FIRST_NAME={firstName}, LAST_NAME={lastName}, POSITION={position}, LOCATION={location}
          where ID = {id}
        """
      ).on(
        'id -> resource.id,
        'firstName -> resource.firstName,
        'lastName -> resource.lastName,
        'position -> resource.position,
        'location -> resource.location
      ).executeUpdate()
    }
  }

  /**
   * Insert a new resource.
   *
   * @param resource The resource values.
   */
  def insert(resource: Resource): Int = {

    //Get new ID 
    val currentMaxId = DB.withConnection { implicit connection =>
      SQL("select MAX(ID) from RESOURCES").as(SqlParser.int("MAX(ID)").single)
    }
    
    val newID = currentMaxId + 1

    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into RESOURCES (
            ID, FIRST_NAME, LAST_NAME, POSITION, LOCATION
          ) values (
            {id}, {firstName}, {lastName}, {position}, {location}
          )
        """
      ).on(
        'id -> newID,
        'firstName -> resource.firstName,
        'lastName -> resource.lastName,
        'position -> resource.position,
        'location -> resource.location
      ).executeUpdate()
    }

    //Return the new ID
    newID
  }

  /**
   * Delete a resource.
   *
   * @param id Id of the resource to delete.
   */
  def delete(id: Int) = {
    DB.withConnection { implicit connection =>
      SQL("delete from RESOURCES where ID = {id}").on('id -> id).executeUpdate()
    }
  }

  implicit val resourcesWrites = new Writes[Resource] {
    def writes(resource: Resource) = Json.obj(
      "id" -> resource.id,
      "name" -> (resource.firstName + " " + resource.lastName).toString,
      "firstName" -> resource.firstName,
      "lastName" -> resource.lastName,
      "position" -> resource.position,
      "location" -> resource.location
    )
  }

  implicit val resourcesReads: Reads[Resource] = (
  (JsPath \ "id").read[Int] and
  (JsPath \ "firstName").read[String] and
  (JsPath \ "lastName").read[String] and
  (JsPath \ "position").read[String] and
  (JsPath \ "location").read[String]
  )(Resource.apply _)
  
}

case class Slot(slotID:Int, resourceID:Int, projectID: Int, startDate: Date, endDate: Date, allocationPercentage: Int, slotDescription: String)

object Slot{

  /**
   * Parse a Slot from a ResultSet
   */
  val slotParser = {
    get[Int]("SLOTS.ID") ~
    get[Int]("SLOTS.RESOURCE_ID") ~
    get[Int]("SLOTS.PROJECT_ID") ~
    get[Date]("SLOTS.START_DATE") ~
    get[Date]("SLOTS.END_DATE") ~
    get[Int]("SLOTS.ALLOCATION_PERCENTAGE") ~
    get[String] ("SLOTS.SLOT_DESCRIPTION") map {
      case slotID~resourceID~projectID~startDate~endDate~allocationPercentage~slotDescription => Slot(slotID, resourceID, projectID,
        startDate, endDate, allocationPercentage, slotDescription)
    }
  }

  /**
   * Retrieve all Slots for a project id.
   */
  def findByProjectID(id: Integer): List[Slot] = {
    DB.withConnection { implicit connection =>
      SQL("select * from SLOTS where PROJECT_ID = {id}").on('id -> id).as(slotParser *)
    }
  }

  /**
   * Retrieve all Slot for a resource id.
   */
  def findByResourceID(id: Integer): List[Slot] = {
    DB.withConnection { implicit connection =>
      SQL("select * from SLOTS where RESOURCE_ID = {id}").on('id -> id).as(slotParser *)
    }
  }

  /**
   * Update a slot.
   *
   * @param slot The project values.
   */
  def update(slot: Slot) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update SLOTS
          set RESOURCE_ID={resourceID}, PROJECT_ID={projectID}, START_DATE={startDate}, END_DATE={endDate}, ALLOCATION_PERCENTAGE={allocationPercentage},
          SLOT_DESCRIPTION={slotDescription}
          where ID = {slotID}
        """
      ).on(
        'slotID -> slot.slotID,
        'resourceID -> slot.resourceID,
        'projectID -> slot.projectID,
        'startDate -> slot.startDate,
        'endDate -> slot.endDate,
        'allocationPercentage -> slot.allocationPercentage,
        'slotDescription -> slot.slotDescription
      ).executeUpdate()
    }
  }

  /**
   * Insert a new slot.
   *
   * @param slot The slot values.
   */
  def insert(slot: Slot) = {

    //Get new ID 
    val currentMaxId = DB.withConnection { implicit connection =>
      SQL("select MAX(ID) from SLOTS").as(SqlParser.int("MAX(ID)").single)
    }
    Logger.debug("currentMaxId=" + currentMaxId)
    val newID = currentMaxId + 1

    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into SLOTS (
            ID, PROJECT_ID, RESOURCE_ID, START_DATE, END_DATE, ALLOCATION_PERCENTAGE, SLOT_DESCRIPTION
          ) values (
            {id}, {projectID}, {resourceID}, {startDate}, {endDate}, {allocationPercentage}, {slotDescription}
          )
        """
      ).on(
        'id -> newID,
        'projectID -> slot.projectID,
        'resourceID -> slot.resourceID,
        'startDate -> slot.startDate,
        'endDate -> slot.endDate,
        'allocationPercentage -> slot.allocationPercentage,
        'slotDescription -> slot.slotDescription
      ).executeUpdate()
    }
  }

  /**
   * Delete a slot.
   *
   * @param id Id of the slot to delete.
   */
  def delete(id: Int) = {
    DB.withConnection { implicit connection =>
      SQL("delete from SLOTS where ID = {id}").on('id -> id).executeUpdate()
    }
  }

  implicit val slotsWrites = new Writes[Slot] {
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
    def writes(slot: Slot) = Json.obj(
      "slotID" -> slot.slotID,
      "resourceID" -> slot.resourceID,
      "projectID" -> slot.projectID,
      "startDate" -> format.format(slot.startDate),
      "endDate" -> format.format(slot.endDate),
      "allocationPercentage" -> slot.allocationPercentage,
      "slotDescription" -> slot.slotDescription
    )
  }

  implicit val slotsReads: Reads[Slot] = (
  (JsPath \ "slotID").read[Int] and
  (JsPath \ "resourceID").read[Int] and
  (JsPath \ "projectID").read[Int] and
  (JsPath \ "startDate").read[Date] and
  (JsPath \ "endDate").read[Date] and
  (JsPath \ "allocationPercentage").read[Int] and
  (JsPath \ "slotDescription").read[String] 
  )(Slot.apply _)



}

case class SlotProject(id: Int, resourceID:Int, projectID: Int, projectName: String, startDate: Date, endDate: Date, allocationPercentage: Int)

object SlotProject{

  /**
   * Parse a Slot from a ResultSet
   */
  val slotProjectParser = {
    get[Int]("SLOTS.ID") ~
    get[Int]("SLOTS.RESOURCE_ID") ~
    get[Int]("SLOTS.PROJECT_ID") ~
    get[String]("PROJECTS.PROJECT_NAME") ~
    get[Date]("SLOTS.START_DATE") ~
    get[Date]("SLOTS.END_DATE") ~
    get[Int]("SLOTS.ALLOCATION_PERCENTAGE") map {
      case id~resourceID~projectID~projectName~startDate~endDate~allocationPercentage => SlotProject(id,resourceID, projectID, projectName, startDate, endDate, allocationPercentage)
    }
  }

  /**
   * Retrieve all Slots for a project id.
   */
  def findByProjectID(id: Integer): List[SlotProject] = {
    DB.withConnection { implicit connection =>
      SQL("select * from SLOTS where PROJECT_ID = {id}").on('id -> id).as(slotProjectParser *)
    }
  }

  /**
   * Retrieve all Slot for a resource id.
   */
  def findByResourceID(id: Integer): List[SlotProject] = {
    DB.withConnection { implicit connection =>
      SQL("select * from SLOTS, PROJECTS where SLOTS.RESOURCE_ID = {id} AND SLOTS.PROJECT_ID = PROJECTS.ID").on('id -> id).as(slotProjectParser *)
    }
  }

  implicit val slotsWrites = new Writes[SlotProject] {
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
    def writes(slot: SlotProject) = Json.obj(
      "id" -> slot.id,
      "resourceID" -> slot.resourceID,
      "projectID" -> slot.projectID,
      "projectName" -> slot.projectName,
      "startDate" -> format.format(slot.startDate),
      "endDate" -> format.format(slot.endDate),
      "allocationPercentage" -> slot.allocationPercentage
    )
  }

}


case class ResourceSlots(id: Int, firstName: String, lastName: String, position: String, location:String, slots: List[SlotProject])

object ResourceSlots{

  /**
   * Parse a Resource from a ResultSet
   */
  val resourceSlotsParser = {
    get[Int]("RESOURCES.ID") ~
    get[String]("RESOURCES.FIRST_NAME") ~
    get[String]("RESOURCES.LAST_NAME") ~
    get[String]("RESOURCES.POSITION") ~
    get[String]("RESOURCES.LOCATION") map {
      //case id~firstName~lastName~position~location => ResourceSlots(id, firstName, lastName, position, location, List(Slot(1,1,new Date(),new Date(),20)))
      case id~firstName~lastName~position~location => ResourceSlots(id, firstName, lastName, position, location, SlotProject.findByResourceID(id))
    }
  }

  /**
   * Retrieve a Resource from the id.
   */
  def findById(id: Integer): Option[ResourceSlots] = {
    DB.withConnection { implicit connection =>
      SQL("select * from RESOURCES where ID = {id}").on('id -> id).as(resourceSlotsParser.singleOpt)
    }
  }

  /**
   * Retrieve all resources
   *
   */
  def listAll: List[ResourceSlots]={
    DB.withConnection { implicit connection =>
      SQL("select * from RESOURCES").as(resourceSlotsParser *)
    }
  }

  implicit val resourcesWrites = new Writes[ResourceSlots] {
    def writes(resource: ResourceSlots) = Json.obj(
      "id" -> resource.id,
      "firstName" -> resource.firstName,
      "lastName" -> resource.lastName,
      "position" -> resource.position,
      "location" -> resource.location,
      "slots" -> resource.slots
    )
  }
  
}

case class ProjectResourceSlots(resourceID: Int, firstName: String, lastName: String, position: String, location:String, 
                                slotID: Int, projectID: Int, startDate: Date, endDate: Date, allocationPercentage: Int, slotDescription: String)

object ProjectResourceSlots{

  /**
   * Parse a Resource from a ResultSet
   */
  val projectResourceSlotsParser = {
    get[Int]("RESOURCES.ID") ~
    get[String]("RESOURCES.FIRST_NAME") ~
    get[String]("RESOURCES.LAST_NAME") ~
    get[String]("RESOURCES.POSITION") ~
    get[String]("RESOURCES.LOCATION") ~
    get[Int]("SLOTS.ID") ~
    get[Int]("SLOTS.PROJECT_ID") ~
    get[Date]("SLOTS.START_DATE") ~
    get[Date]("SLOTS.END_DATE") ~
    get[Int]("SLOTS.ALLOCATION_PERCENTAGE") ~
    get[String]("SLOTS.SLOT_DESCRIPTION") map {
      case resourceID~firstName~lastName~position~location~slotID~projectID~startDate~endDate~allocationPercentage~slotDescription => 
        ProjectResourceSlots(resourceID, firstName, lastName, position, location, slotID, projectID, startDate, endDate, allocationPercentage, slotDescription)
    }
  }

  /**
   * Retrieve a Resource from the id.
   */
  /*def findById(id: Integer): Option[ProjectResourceSlots] = {
    DB.withConnection { implicit connection =>
      SQL("select * from RESOURCES where ID = {id}").on('id -> id).as(resourceSlotsParser.singleOpt)
    }
  }*/

  def findByProjectId(projectID: Integer): List[ProjectResourceSlots] = {
    DB.withConnection { implicit connection =>
      SQL("select * from RESOURCES,SLOTS where SLOTS.PROJECT_ID = {id} AND SLOTS.RESOURCE_ID = RESOURCES.ID").on('id -> projectID).as(projectResourceSlotsParser *)
    }
  }

  /**
   * Retrieve all resources
   *
   */
  /*def listAll: List[ProjectResourceSlots]={
    DB.withConnection { implicit connection =>
      SQL("select * from RESOURCES").as(resourceSlotsParser *)
    }
  }*/

  implicit val resourcesWrites = new Writes[ProjectResourceSlots] {
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
    def writes(resource: ProjectResourceSlots) = Json.obj(
      "resourceID" -> resource.resourceID,
      "firstName" -> resource.firstName,
      "lastName" -> resource.lastName,
      "position" -> resource.position,
      "location" -> resource.location,
      "slotID" -> resource.slotID,
      "projectID" -> resource.projectID,
      "startDate" -> format.format(resource.startDate),
      "endDate" -> format.format(resource.endDate),
      "allocationPercentage" -> resource.allocationPercentage,
      "slotDescription" -> resource.slotDescription
    )
  }
  
}

case class Serie(name: String, data:Array[Float])

object Serie {

  implicit val resourcesWrites = new Writes[Serie] {
    def writes(serieEntry: Serie) = Json.obj(
      "name" -> serieEntry.name,
      "data" -> serieEntry.data
    )
  }
}

case class GraphEntry(columns: ListBuffer[String],series: ListBuffer[Serie])

object GraphEntry {
  implicit val resourcesWrites = new Writes[GraphEntry] {
    def writes(graphEntry: GraphEntry) = Json.obj(
      "columns" -> graphEntry.columns,
      "series" -> graphEntry.series
    )
  }
}

case class ProjectDetails(id: Int, projectName: String, customerName: String, iataCode: String, highLevelScope:String,
 sizing: Int, resources: List[ProjectResourceSlots], var resourcesAllocatedMD: Float, graphEntry: GraphEntry){
  /**
   * Calculate current time allocation
   */
  def calculateTotalMDAllocated() = {
    var earliestDate: Date = null
    var latestDate: Date = null

    resources.map { resource =>
      
      if (earliestDate==null){
        earliestDate = resource.startDate
        latestDate = resource.endDate
      }
      else{
        if (earliestDate.compareTo(resource.startDate) > 0 ) earliestDate = resource.startDate
        if (latestDate.compareTo(resource.endDate) < 0) latestDate = resource.endDate
      }

 
    }

    //TODO: compare earliestDate and latest to wished dates to extand the range if necessary

    var currentDate = earliestDate;
    var numberOfDays = 0;
    var numberOfMD: Float = 0;

    while (currentDate.compareTo(latestDate) <= 0) {
      numberOfDays = numberOfDays + 1

      val cal = Calendar.getInstance();
      cal.setTime(currentDate);
      val dayOfWeek: Int = cal.get(Calendar.DAY_OF_WEEK);

      if ((dayOfWeek > 1) && (dayOfWeek < 7)) //It's a working day... for Western countries at least
        resources.map { resource =>
            if ((currentDate.compareTo(resource.startDate) >= 0 ) && (currentDate.compareTo(resource.endDate) <= 0)) {
              numberOfMD = numberOfMD + (resource.allocationPercentage.toFloat / 100)
              
            }

        }

      //Add one day to currentDate
      cal.add(Calendar.DATE, 1); //minus number would decrement the days
      currentDate = cal.getTime();
      //End add one day to current date
    }

    resourcesAllocatedMD = numberOfMD
  }

  /**
   * Create Graph and calculate time allocation?
   */
  def calculateTotalMDAllocatedAndGraph() = {
    var earliestDate: Date = null
    var latestDate: Date = null
    val personsList = ListBuffer[String]()
    var atLeastOneResourceExist = false

    resources.map { resource =>

      atLeastOneResourceExist = true
      
      //Check dates
      if (earliestDate==null){
        earliestDate = resource.startDate
        latestDate = resource.endDate
      }
      else{
        if (earliestDate.compareTo(resource.startDate) > 0 ) earliestDate = resource.startDate
        if (latestDate.compareTo(resource.endDate) < 0) latestDate = resource.endDate
      }

      //Create list of rersources for Graph
      val resourceName = resource.firstName+" "+resource.lastName
      if (!personsList.contains(resourceName)) personsList += resourceName

 
    }

    if (atLeastOneResourceExist){
      //TODO: compare earliestDate and latest to wished dates to extand the range if necessary

      //Calculate the duration of the project
      val diffInMillies = latestDate.getTime() - earliestDate.getTime()
      val nbDays = TimeUnit.DAYS.convert(diffInMillies,TimeUnit.MILLISECONDS) + 1

      //We have the list of people working on the project, let's build the objects for the graph
      personsList.map{ person =>
        graphEntry.series += Serie(person,Array.fill(nbDays.toInt)(0))
      }

      var currentDate = earliestDate;
      var numberOfDays = 0;
      var numberOfMD: Float = 0;
      val format = new java.text.SimpleDateFormat("yyyy-MM-dd")

      while (currentDate.compareTo(latestDate) <= 0) {
        numberOfDays = numberOfDays + 1

        graphEntry.columns += format.format(currentDate)

        val cal = Calendar.getInstance();
        cal.setTime(currentDate);
        val dayOfWeek: Int = cal.get(Calendar.DAY_OF_WEEK);

        if ((dayOfWeek > 1) && (dayOfWeek < 7)) //It's a working day... for Western countries at least
          resources.map { resource =>
              if ((currentDate.compareTo(resource.startDate) >= 0 ) && (currentDate.compareTo(resource.endDate) <= 0)) {
                // Calculate total number of MD for the project
                numberOfMD = numberOfMD + (resource.allocationPercentage.toFloat / 100)
                //Calculate daily allocation.
                val personInGraphList = graphEntry.series.find(a => a.name == (resource.firstName+" "+resource.lastName))
                personInGraphList match {
                  case Some(person) => person
                    person.data(numberOfDays-1) += (resource.allocationPercentage.toFloat / 100)
                  case None => 
                }

              }

          }

        //Add one day to currentDate
        cal.add(Calendar.DATE, 1); //minus number would decrement the days
        currentDate = cal.getTime();
        //End add one day to current date
      }

      resourcesAllocatedMD = numberOfMD
    }
  }
}

object ProjectDetails{

  /**
   * Parse a Resource from a ResultSet
   */
  val projectDetailsParser = {
    get[Int]("PROJECTS.ID") ~
    get[String]("PROJECTS.PROJECT_NAME") ~
    get[String]("PROJECTS.CUSTOMER_NAME") ~
    get[String]("PROJECTS.IATA_CODE") ~
    get[String]("PROJECTS.HIGH_LEVEL_SCOPE") ~
    get[Int]("PROJECTS.ESTIMATED_SIZING") map {
      case id~projectName~customerName~iataCode~highLevelScope~sizing =>
        ProjectDetails(id, projectName, customerName, iataCode, highLevelScope, sizing,
          ProjectResourceSlots.findByProjectId(id), 0, GraphEntry(ListBuffer[String](),ListBuffer[Serie]()))
    }
  }

  /**
   * Retrieve a Resource from the id.
   */
  def findById(id: Integer): Option[ProjectDetails] = {
    DB.withConnection { implicit connection =>
      SQL("select ID,PROJECT_NAME,CUSTOMER_NAME,IATA_CODE,HIGH_LEVEL_SCOPE,ESTIMATED_SIZING from PROJECTS where ID = {id}").on('id -> id).as(projectDetailsParser.singleOpt)
    }
  }



  implicit val projectDetailsWrites = new Writes[ProjectDetails] {
    def writes(project: ProjectDetails) = Json.obj(
      "id" -> project.id,
      "projectName" -> project.projectName,
      "customerName" -> project.customerName,
      "iataCode" -> project.iataCode,
      "highLevelScope" -> project.highLevelScope,
      "sizing" -> project.sizing,
      "resources" -> project.resources,
      "resourcesAllocatedMD" -> project.resourcesAllocatedMD,
      "graphEntry" -> project.graphEntry
    )
  }
  
}

case class Project(id: Int, projectName: String, customerName: String, customerType: String, iataCode: String, highLevelScope: String, cr: String, priority: String,
                  slotStatus: String, slotRequestDate: Date, questionaireFiled: Boolean,
                  businessCaseFiled: Boolean, estimatedSizing: Int, wishedStartDate: Date,
                  wishedEndDate: Date, slotExpiryDate: Date, status: String)

object Project{

  /**
   * Parse a Project from a ResultSet
   */
  val projectSlotsParser = {
    get[Int]("PROJECTS.ID") ~
    get[String]("PROJECTS.PROJECT_NAME") ~
    get[String]("PROJECTS.CUSTOMER_NAME") ~
    get[String]("PROJECTS.CUSTOMER_TYPE") ~
    get[String]("PROJECTS.IATA_CODE")  ~
    get[String]("PROJECTS.HIGH_LEVEL_SCOPE")  ~
    get[String]("PROJECTS.CR")  ~
    get[String]("PROJECTS.PRIORITY")  ~
    get[String]("PROJECTS.SLOT_STATUS")  ~
    get[Date]("PROJECTS.SLOT_REQUEST_DATE")  ~
    get[Boolean]("PROJECTS.QUESTIONAIRE_FILED")  ~
    get[Boolean]("PROJECTS.BUSINESS_CASE_FILED")  ~
    get[Int]("PROJECTS.ESTIMATED_SIZING")  ~
    get[Date]("PROJECTS.WISHED_START_DATE")  ~
    get[Date]("PROJECTS.WISHED_END_DATE")  ~
    get[Date]("PROJECTS.SLOT_EXPIRY_DATE")  ~
    get[String]("PROJECTS.STATUS") map {
      case id~projectName~customerName~customerType~iataCode~highLevelScope~cr~
          priority~slotStatus~slotRequestDate~questionaireFiled~businessCaseFiled~
          estimatedSizing~wishedStartDate~wishedEndDate~slotExpiryDate~status
      => Project(id, projectName, customerName, customerType, iataCode, highLevelScope, cr,
                priority,slotStatus,slotRequestDate,questionaireFiled,businessCaseFiled,
                estimatedSizing,wishedStartDate,wishedEndDate,slotExpiryDate,status
      )
    }
  }

  /**
   * Retrieve all projects
   *
   */
  def listAll: List[Project]={
    DB.withConnection { implicit connection =>
      SQL("select * from PROJECTS").as(projectSlotsParser *)
    }
  }

  /**
   * Insert a new project.
   *
   * @param project The project values.
   */
  def insert(project: Project) = {

    //Get new ID 
    val currentMaxId = DB.withConnection { implicit connection =>
      SQL("select MAX(ID) from PROJECTS").as(SqlParser.int("MAX(ID)").single)
    }
    Logger.debug("currentMaxId=" + currentMaxId)
    val newID = currentMaxId + 1

    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into PROJECTS (
            ID, PROJECT_NAME, CUSTOMER_NAME, CUSTOMER_TYPE, IATA_CODE, HIGH_LEVEL_SCOPE,
            CR, PRIORITY, SLOT_STATUS, SLOT_REQUEST_DATE, QUESTIONAIRE_FILED, BUSINESS_CASE_FILED,
            ESTIMATED_SIZING, WISHED_START_DATE, WISHED_END_DATE, SLOT_EXPIRY_DATE, STATUS
          ) values (
            {id}, {projectName}, {customerName}, {customerType}, {iataCode}, {highLevelScope}, {cr},
            {priority},{slotStatus},{slotRequestDate},{questionaireFiled},{businessCaseFiled},
            {estimatedSizing}, {wishedStartDate}, {wishedEndDate}, {slotExpiryDate}, {status}
          )
        """
      ).on(
        'id -> newID,
        'projectName -> project.projectName,
        'customerName -> project.customerName,
        'customerType -> project.customerType,
        'iataCode -> project.iataCode,
        'highLevelScope -> project.highLevelScope,
        'cr -> project.cr,
        'priority -> project.priority,
        'slotStatus -> project.slotStatus,
        'slotRequestDate -> project.slotRequestDate,
        'questionaireFiled -> project.questionaireFiled,
        'businessCaseFiled -> project.businessCaseFiled,
        'estimatedSizing -> project.estimatedSizing,
        'wishedStartDate -> project.wishedStartDate,
        'wishedEndDate -> project.wishedEndDate,
        'slotExpiryDate -> project.slotExpiryDate,
        'status -> project.status
      ).executeUpdate()
    }
  }
  /**
   * Update a project.
   *
   * @param project The project values.
   */
  def update(project: Project) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update PROJECTS
          set PROJECT_NAME={projectName}, CUSTOMER_NAME={customerName}, CUSTOMER_TYPE={customerType},
            IATA_CODE={iataCode}, HIGH_LEVEL_SCOPE={highLevelScope}, CR={cr},
            PRIORITY={priority}, SLOT_STATUS={slotStatus}, SLOT_REQUEST_DATE={slotRequestDate},
            QUESTIONAIRE_FILED={questionaireFiled},BUSINESS_CASE_FILED={businessCaseFiled},
            ESTIMATED_SIZING={estimatedSizing}, WISHED_START_DATE={wishedStartDate}, WISHED_END_DATE={wishedEndDate},
            SLOT_EXPIRY_DATE={slotExpiryDate}, STATUS={status}
          where id = {id}
        """
      ).on(
        'id -> project.id,
        'projectName -> project.projectName,
        'customerName -> project.customerName,
        'customerType -> project.customerType,
        'iataCode -> project.iataCode,
        'highLevelScope -> project.highLevelScope,
        'cr -> project.cr,
        'priority -> project.priority,
        'slotStatus -> project.slotStatus,
        'slotRequestDate -> project.slotRequestDate,
        'questionaireFiled -> project.questionaireFiled,
        'businessCaseFiled -> project.businessCaseFiled,
        'estimatedSizing -> project.estimatedSizing,
        'wishedStartDate -> project.wishedStartDate,
        'wishedEndDate -> project.wishedEndDate,
        'slotExpiryDate -> project.slotExpiryDate,
        'status -> project.status
      ).executeUpdate()
    }
  }

  /**
   * Delete a project.
   *
   * @param id Id of the project to delete.
   */
  def delete(id: Int) = {
    DB.withConnection { implicit connection =>
      SQL("delete from PROJECTS where ID = {id}").on('id -> id).executeUpdate()
    }
  }



  implicit val projectsWrites = new Writes[Project] {
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
    def writes(project: Project) = Json.obj(
      "id" -> project.id,
      "projectName" -> project.projectName,
      "customerName" -> project.customerName,
      "customerType" -> project.customerType,
      "iataCode" -> project.iataCode,
      "highLevelScope" -> project.highLevelScope,
      "cr" -> project.cr,
      "priority" -> project.priority,
      "slotStatus" -> project.slotStatus,
      "slotRequestDate" -> format.format(project.slotRequestDate),
      "questionaireFiled" -> project.questionaireFiled,
      "businessCaseFiled" -> project.businessCaseFiled,
      "estimatedSizing" -> project.estimatedSizing,
      "wishedStartDate" -> format.format(project.wishedStartDate),
      "wishedEndDate" -> format.format(project.wishedEndDate),
      "slotExpiryDate" -> format.format(project.slotExpiryDate),
      "status" -> project.status
    )
  }

  implicit val projectReads: Reads[Project] = (
  (JsPath \ "id").read[Int] and
  (JsPath \ "projectName").read[String] and
  (JsPath \ "customerName").read[String] and
  (JsPath \ "customerType").read[String] and
  (JsPath \ "iataCode").read[String] and
  (JsPath \ "highLevelScope").read[String] and
  (JsPath \ "cr").read[String] and
  (JsPath \ "priority").read[String] and
  (JsPath \ "slotStatus").read[String] and
  (JsPath \ "slotRequestDate").read[Date] and
  (JsPath \ "questionaireFiled").read[Boolean] and
  (JsPath \ "businessCaseFiled").read[Boolean] and
  (JsPath \ "estimatedSizing").read[Int] and
  (JsPath \ "wishedStartDate").read[Date] and
  (JsPath \ "wishedEndDate").read[Date] and
  (JsPath \ "slotExpiryDate").read[Date] and
  (JsPath \ "status").read[String]
  )(Project.apply _)
}


