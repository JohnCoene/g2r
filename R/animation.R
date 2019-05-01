#' Animations
#'
#' Helper function to build animations.
#' 
#' @examples
#' # create animation
#' anim <- Animation$
#'   new()$
#'   appear(duration = 2000, delay = 500)
#' 
#' iris %>% 
#'   g2(asp(Sepal.Length, Sepal.Width, color = Species)) %>% 
#'   fig_point(anim) # pass animation to the geom
#' 
#' @name G2animation
#' @export
Animation <- R6::R6Class(
  "Animation",
  public = list(
    enter = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL){
      private$.enter <- private$build_list(animation, easing, delay, duration)
      invisible(self)
    },
    leave = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL){
      private$.leave <- private$build_list(animation, easing, delay, duration)
      invisible(self)
    },
    appear = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL){
      private$.appear <- private$build_list(animation, easing, delay, duration)
      invisible(self)
    },
    update = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL){
      private$.update <- private$build_list(animation, easing, delay, duration)
      invisible(self)
    },
    print = function(){
      print("A g2r figure animation")
      invisible(self)
    }
  ),
  active = list(
    get_list = function(){
      anim <- list(
        enter = private$.enter,
        leave = private$.leave,
        appear = private$.appear,
        update = private$.update
      )
      anim %>% 
        keep(
          function(x){
            !is.null(x)
          }
        )
    }
  ),
  private = list(
    .enter = NULL,
    .leave = NULL,
    .appear = NULL,
    .update = NULL,
    build_list = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL){

      anim <- list()

      if(!is.null(animation))
        anim$animation <- animation

      if(!is.null(easing))
        anim$easing <- easing

      if(!is.null(delay))
        anim$delay <- delay

      if(!is.null(duration))
        anim$duration <- duration
      
      return(anim)
    }
  )
)

#' @rdname G2animation
#' @export
new_animation <- function(){
  Animation$new()
}

# is animation to keep
is_animation <- function(x){
  aes <- FALSE
  if(inherits(x, "Animation"))
    aes <- TRUE
  return(aes)
}

# retrieve aesthetics
get_animation <- function(...){
  anim <- list(...) %>% 
    keep(is_animation) 

  if(length(anim))
    anim[[1]]$get_list
  else
    NULL
}