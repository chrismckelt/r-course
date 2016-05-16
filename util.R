saveFile = function(url,name){
  url <- url
  filename <- name
  download.file(url, filename)
}