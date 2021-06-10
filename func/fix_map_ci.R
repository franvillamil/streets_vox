# Function to fix coordinates (Canary Islands)
# Borrowed from https://stackoverflow.com/questions/13757771
fix1 = function(object, params){
  r = params[1]
  scale = params[2]
  shift = params[3:4]
  object = elide(object, rotate = r)
  size = max(apply(bbox(object), 1, diff))/scale
  object = elide(object, scale = size)
  object = elide(object, shift = shift)
  object
}
