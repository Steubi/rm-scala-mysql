package controllers

import play.api._
import play.api.mvc._

import models._

import play.api.libs.json._
import java.util.{Date}
import java.util.{Calendar}

import scala.concurrent.Future
import scala.concurrent._
import ExecutionContext.Implicits.global

import org.joda.time.DateTime;
import org.joda.time.Days;



class Application extends Controller {

  def index = Action { 

  	Ok(views.html.rm_index())
    
  }

  def getAllResourcesSlots() = Action {
  	val resourcesProjects = ResourceSlots.listAll

  	Ok(Json.toJson(resourcesProjects).toString())
  }

  def getAllResources() = Action {
    val resources = Resource.listAll

    Ok(Json.toJson(resources).toString())
  }

  def getAllProjects() = Action {
    //val testProject = new Project(99,"Test","","","","","","","",new Date(),false,false,0,new Date(),new Date(),new Date(),"")

    //Project.insert(testProject)

    val projects = Project.listAll
    Ok(Json.toJson(projects).toString())
  }

  def projects = Action{
    Ok(views.html.rm())
  }

  def deleteProject = Action.async(parse.json) { request =>
    val myProjectID = request.body \ "id"
    Project.delete(myProjectID.as[Int])
    Future(Created)

  }

  def saveProject = Action.async(parse.json) { request =>
    /*
     * request.body is a JsValue.
     * There is an implicit Writes that turns this JsValue as a JsObject,
     * so you can call insert() with this JsValue.
     * (insert() takes a JsObject as parameter, or anything that can be
     * turned into a JsObject using a Writes.)
     */

    // Get the projectID passed by the POST request. We need to check if it's a new project or not.
    Logger.debug("Request:" + request.body.toString())
    //val myProjectID = (request.body \ "id")
    val projectResult: JsResult[Project] = request.body.validate[Project]

    // Pattern matching
    projectResult match {
      case s: JsSuccess[Project] => {
        Logger.debug("Project Object: " + s.value)
        if (s.value.id == -1){
          Logger.debug("New project!!!")
          Project.insert(s.value);
        }
        else{
          Logger.debug("Existing project!!!")
          Project.update(s.value);
        }
        //Project.insert(s.value);
        Future(Created)
      }
      case e: JsError => {
        Logger.debug("ERROR:" + e.toString)
        Future(BadRequest("Record not found"))
      }
    }
        
  }

  def projectPage(projectID: Int) = Action{
    Ok(views.html.projectPage(projectID))
  }

  def projectDetails(projectID: Int) = Action {
    
    val project = ProjectDetails.findById(projectID)


    project match {
      case Some(project) => project
        project.calculateTotalMDAllocatedAndGraph
        Ok(Json.toJson(project).toString())
      case None => BadRequest("Record not found")
    }


    
  }


  def saveSlot = Action.async(parse.json) { request =>
    /*
     * request.body is a JsValue.
     * There is an implicit Writes that turns this JsValue as a JsObject,
     * so you can call insert() with this JsValue.
     * (insert() takes a JsObject as parameter, or anything that can be
     * turned into a JsObject using a Writes.)
     */

    // Get the projectID passed by the POST request. We need to check if it's a new project or not.
    Logger.debug("Request:" + request.body.toString())
    //val myProjectID = (request.body \ "id")
    val slotResult: JsResult[Slot] = request.body.validate[Slot]

    // Pattern matching
    slotResult match {
      case s: JsSuccess[Slot] => {
        Logger.debug("Project Object: " + s.value)  
        Slot.update(s.value);
        //Project.insert(s.value);
        Future(Created)
      }
      case e: JsError => {
        Logger.debug("ERROR:" + e.toString)
        Future(BadRequest("Record not found"))
      }
    }
        
  }

  def saveNewSlot = Action.async(parse.json) { request =>
    /*
     * request.body is a JsValue.
     * There is an implicit Writes that turns this JsValue as a JsObject,
     * so you can call insert() with this JsValue.
     * (insert() takes a JsObject as parameter, or anything that can be
     * turned into a JsObject using a Writes.)
     */

    // Get the projectID passed by the POST request. We need to check if it's a new project or not.
    Logger.debug("Request:" + request.body.toString())
    //val myProjectID = (request.body \ "id")
    val projectResult: JsResult[Slot] = request.body.validate[Slot]

    // Pattern matching
    projectResult match {
      case s: JsSuccess[Slot] => {
        Logger.debug("Project Object: " + s.value)  
        Slot.insert(s.value);
        //Project.insert(s.value);
        Future(Created)
      }
      case e: JsError => {
        Logger.debug("ERROR:" + e.toString)
        Future(BadRequest("Record not found"))
      }
    }
        
  }

  def deleteSlot = Action.async(parse.json) { request =>
    val mySlotID = request.body \ "slotID"
    Slot.delete(mySlotID.as[Int])
    Future(Ok)

  }

  def resourcesPage() = Action{
    Ok(views.html.resourcePage())
  }

  def saveResource = Action.async(parse.json) { request =>
    /*
     * request.body is a JsValue.
     * There is an implicit Writes that turns this JsValue as a JsObject,
     * so you can call insert() with this JsValue.
     * (insert() takes a JsObject as parameter, or anything that can be
     * turned into a JsObject using a Writes.)
     */

    
    val resourceResult: JsResult[Resource] = request.body.validate[Resource]

    // Pattern matching
    resourceResult match {
      case s: JsSuccess[Resource] => {
        if (s.value.id == -1){
          val newID = Resource.insert(s.value);
          // We need to return the new ID
          //TODO: Do the same for others inserts: New project: New slot...
          Future(Created(Json.obj("id" -> newID)))
        }
        else{
          Resource.update(s.value);
          //TODO: Check all return code...
          Future(Created)
        }
        
      }
      case e: JsError => {
        Logger.debug("ERROR:" + e.toString)
        Future(BadRequest("Record not found"))
      }
    }
        
  }

  def deleteResource = Action.async(parse.json) { request =>
    val myID = request.body \ "id"
    Resource.delete(myID.as[Int])
    Future(Ok)
  }

}
