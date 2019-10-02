area_circ <- function (r) pi * r^2
area_rect <- function (l, w) l * w

parse_area <- function (string) {
  area <- dplyr::case_when(
    string == "10m radius" ~ area_circ(10),
    string == "indefinite; ca. 40 m radius" ~ area_circ(40),
    string == "6m radius" ~ area_circ(6),
    string == "25m x 50m" ~ area_rect(25, 50),
    string == "25m radius" ~ area_circ(25),
    string == "300m radius" ~ area_circ(300),
    string == "50m x 150m" ~ area_rect(50, 150),
    string == "0" ~ 0,
    string == "20m radius" ~ area_circ(20),
    string == "50m radius" ~ area_circ(50),
    string == "100m radius" ~ area_circ(100),
    string == "25m x 150m" ~ area_rect(25, 150),
    string == "ca. 60m radius" ~ area_circ(60),
    string == "5m radius" ~ area_circ(5),
    string == "ca. 15m radius" ~ area_circ(15),
    string == "40m radius" ~ area_circ(40),
    string == "25m x 55m" ~ area_rect(25, 55),
    string == "15m radius" ~ area_circ(15),
    string == "75m radius" ~ area_circ(75),
    string == "30m radius" ~ area_circ(30),
    string == "20m x 20m" ~ area_rect(20, 20),
    string == "15 m radius"  ~ area_circ(15)
  )

  return(area)
}
