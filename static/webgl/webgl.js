
var RSGL = {};

RSGL.GOLDEN_RATIO = 1.618;
RSGL.TAU = 2*Math.PI;

RSGL.DEFAULT_COMPASS = {
  body : {
    inner_diameter : 10,
    inner_thickness : 1,
    rim_diameter : 12,
    rim_thickness : 1,
    color : 0xffffff,
  },
  
  needle : {
    needle_width : 1,
    needle_length : 9,
    color : 0xff0000,
  }
};

RSGL.facingToRadians = function( str )
{
  str = str.toLowerCase();
  
  if( str === "north" )
    return 0;
  if( str === "northeast" )
    return 1/8*RSGL.TAU;
  if( str === "east" )
    return 2/8*RSGL.TAU;
  if( str === "southeast" )
    return 3/8*RSGL.TAU;
  if( str === "south" )
    return 4/8*RSGL.TAU;
  if( str === "southwest" )
    return 5/8*RSGL.TAU;
  if( str === "west" )
    return 6/8*RSGL.TAU;
  if( str === "northwest" )
    return 7/8*RSGL.TAU;

  return 0;
}

RSGL.createCompass = function( compass_description )
{
  var compass_body = RSGL.createCompassBody( compass_description.body );
  var compass_needle = RSGL.createCompassNeedle( compass_description.needle );

  compass_needle.position.z = 0.1;
  compass_needle.rotation.z = Math.PI;

  return function(scene, environment) {
    var animated_needle = new THREE.Object3D();
    animated_needle.add(compass_needle);
    animated_needle.applyMatrix(new THREE.Matrix4().makeRotationZ( -RSGL.facingToRadians( environment.payload.compass ) ) );
    
    scene.add( compass_body );
    scene.add( animated_needle );
  };
}

RSGL.createCompassBody = function( compass_body_description )
{
  //Unpacking the parameters as a way of documenting them:
  var inner_thickness = compass_body_description.inner_thickness;
  var inner_diameter =  compass_body_description.inner_diameter;
  var rim_diameter =    compass_body_description.rim_diameter;
  var rim_thickness =   compass_body_description.rim_thickness;
  var color =           compass_body_description.color;

  var pts = [
      new THREE.Vector3(0, 0, -inner_thickness),
      new THREE.Vector3(inner_diameter, 0, -inner_thickness),
      new THREE.Vector3(rim_diameter, 0, 0),
      new THREE.Vector3(rim_diameter, 0, rim_thickness),
      new THREE.Vector3(inner_diameter, 0, rim_thickness),
      new THREE.Vector3(inner_diameter, 0, 0),
      new THREE.Vector3(0, 0, 0)
    ];
     
  var geometry = new THREE.LatheGeometry( pts, 24 );  
  
  var material = new THREE.MeshPhongMaterial( { color: color, shading:THREE.FlatShading } );
  
  var mesh = new THREE.Mesh( geometry, material );

  return mesh;
}

RSGL.createCompassNeedle = function( needle_description ) {
  //Unpacking the parameters as a way of documenting them:
  var needle_width = needle_description.needle_width;
  var needle_length = needle_description.needle_length;
  var color = needle_description.color;

  var needle_shape = new THREE.Shape();
  
  needle_shape.moveTo( -needle_width,0 );
  needle_shape.lineTo( needle_width, 0 );
  needle_shape.lineTo( 0, needle_length );
  needle_shape.lineTo( -needle_width, 0 );
  
  var needle_geom = new THREE.ShapeGeometry( needle_shape );
  return new THREE.Mesh( needle_geom, new THREE.MeshPhongMaterial( { color: 0xff0000 } ) );
}

RSGL.createCamera = function(width, height) {
  var camera = new THREE.PerspectiveCamera(
    65,      //fov degrees
    width/height, //aspect ratio
    1,     //near
    100    //far
    );

  camera.position.set( 0, 20, 20/RSGL.GOLDEN_RATIO );
  camera.up.set( 0, 0, 1 );
  
  return function(scene, environment) {
    camera.lookAt( scene.position );
    return camera;
  }
}

RSGL.createLighting = function() {
  var sunlight = new THREE.SpotLight( 0xAAAA88 ); 
  sunlight.position.set( -15, 0, 15 );

  var skylight = new THREE.HemisphereLight( 0x666688, 0x663300 );
  skylight.position.set(0,0,100);
  
  return function( scene, environment ) {
    scene.add( sunlight );
    scene.add( skylight );
  };
}

RSGL.initialize = function(container) {
  var width = container.width();
  var height = container.height();
  if( height < width / 2 )
    height = width / 2;

  var environment = {
    payload : JSON.parse(container.find('.webgl-payload').html() )};
  
  console.log( "Payload is: " + JSON.stringify(environment) );
  
  var renderer = new THREE.WebGLRenderer();
  var scene = new THREE.Scene();
  renderer.setSize(width, height);
  
  container.replaceWith(renderer.domElement);
  
  RSGL.createLighting()(scene, environment);
  RSGL.createCompass( RSGL.DEFAULT_COMPASS )(scene, environment);

  renderer.render( scene, RSGL.createCamera(width, height)(scene, environment) );
}

$('.webgl-container').each(function(i,container){ RSGL.initialize($(container)) });
