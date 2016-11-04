library(jsonlite)
library(market)

# Methods:
# retrieve_meeting(109501)
# retrieve_event(1175769)

ee <- new.env()
ee_meeting_events<-''
ee_meeting<-''
ee_meeting_id<-''

ee_event=''
ee_event_id=''

#' Retrieve meeting events
#'
#' @param meetingId meeting ID to use for retrival
#' @keywords events
#' @export
#' @examples
#' retrieve_meeting()
retrieve_meeting<-function(meetingId) {
  ee_meeting <- get("ee_meeting", envir = ee)
  ee_meeting_id <- get("ee_meeting_id", envir = ee)

  if( meetingId==ee_meeting_id ) {
    meeting<-ee_meeting
  } else {
    api_response <- jsonlite::fromJSON(paste('http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/meetings/',meetingId,sep=''))
    meeting = eval(parse(text=paste('api_response$data$meetings$`',meetingId, '`',sep="")))
    assign("ee_meeting", meeting, envir = ee)
    assign("ee_meeting_id", meetingId, envir = ee)
  }

  return(meeting)
}

#' Retrieve event IDS
#'
#' @param meetingId meeting ID to use for retrival
#' @keywords events
#' @export
#' @examples
#' retrieve_events()
retrieve_events<-function(meetingId){
  meeting <- retrieve_meeting(meetingId)
  events<-names(eval(parse(text=paste('meeting$events',sep=""))))
  master<-data.frame(matrix(NA,length(events),2))
  colnames(master)<-c('EventID','Race')
  master$EventID<-events
  return(master)
}


#' Retrieve meeting events
#'
#' @param meetingId meeting ID to use for retrival
#' @keywords events
#' @export
#' @examples
#' retrieve_meeting()
retrieve_event<-function(eventId) {
  ee_event <- get("ee_event", envir = ee)
  ee_event_id <- get("ee_event_id", envir = ee)

  if( eventId==ee_event_id ) {
    event<-ee_event
  } else {
    api_response <- jsonlite::fromJSON(paste('http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/events/',eventId,sep=''))
    #event<-eval(paste('api_response$data$events$`',eventId,sep=""))
    event = eval(parse(text=paste('api_response$data$events$`',eventId, '`',sep="")))
    assign("ee_event", event, envir = ee)
    assign("ee_event_id", eventId, envir = ee)
  }

  return(event)
}
#' Retrieve event races
#'
#' @param meetingId meeting ID to use for retrival
#' @keywords races
#' @export
#' @examples
#' retrieve_events()
retrieve_races<-function(meetingId,eventids){
  meeting <- retrieve_meeting(meetingId)
  #events<-jsonlite::fromJSON(paste('http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/meetings/',meetingId,sep=""))
  races<-eval(parse(text=paste('meeting$events$`',eventids,'`$number',sep="")))
  return(races)
}

#' Filter Scratchings
#'
#' @param meetingId DM meeting ID
#' @param race Event number
#' @param eventId Event from DB
#' @keywords non-scratchings
#' @export
#' @examples
#' retrieve_runners_ns(109868,1,1179641)
retrieve_runners_ns<-function(meetingId,race,eventId){
  runners<-retrieve_runners(meetingId,race,eventId)
  runners<-runners[runners$Scratched=='FALSE',]
  return(runners)
}

#' Retrieve event competitors
#'
#' @param meetingId DM meeting ID
#' @param race Event number
#' @param eventId Event from DB
#' @keywords competitors
#' @export
#' @examples
#' retrieve_runners(109868,1,1179641)
retrieve_runners<-function(meetingId,race,eventId){
  events <- retrieve_event(eventId)
  #events <- jsonlite::fromJSON(paste("http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/markets?event_number=",race,"&meeting_id=",meetingId,sep=""))
  a<-paste('events$event_competitors')
  runners<-names(eval(parse(text=a)))
  meetids<-names(events$data$meetings)
  master<-data.frame(matrix(NA,length(runners),6))
  master[,2]<-as.numeric(runners)
  colnames(master)<-c('Course','CompID','Matrix','Race','MeetingID','Odds')
  master$Race<-race
  master$MeetingID<-meetingId
  eventId<-rep(eventId,nrow(master))
  master$Scratched<-mapply(competitor::scratched,eventId,master$CompID)
  return(master)
}

#' Retrieve event competitors IDs
#'
#' @param eventId Database ID of the event
#' @keywords competitors
#' @export
#' @examples
#' retrieve_runners( 1175769 )
retrieve_field<-function( eventId ) {
  events <- jsonlite::fromJSON(paste("http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/events/",eventId,sep=""))
  a<-paste('events$data$events$`',eventId,'`$event_competitors',sep="")
  runners<-names(eval(parse(text=a)))
  field<-length(runners)
  return(field)
}

#' Retrieve event distance
#'
#' @param eventId Database ID of the event
#' @keywords event
#' @export
#' @examples
#' event_distance( 1175769 )
event_distance<-function( eventId ) {
  event<-retrieve_event(eventId)
  a<-paste('event$race_data$distance',sep="")
  distance<-eval(parse(text=a))

  return (distance)
}


#' Retrieve event distance
#'
#' @param eventId Database ID of the event
#' @keywords event
#' @export
#' @examples
#' event_status( 1175769 )
event_status<-function( eventId ) {
  event<-retrieve_event(eventId)
  a<-paste('event$status',sep="")
  status<-eval(parse(text=a))

  return (status)
}


#' Retrieve event name
#'
#' @param eventId Database ID of the event
#' @keywords event
#' @export
#' @examples
#' event_name( 1175769 )
event_name<-function( eventId ) {
  event<-retrieve_event(eventId)
  a<-paste('event$name',sep="")
  status<-eval(parse(text=a))

  return (status)
}
