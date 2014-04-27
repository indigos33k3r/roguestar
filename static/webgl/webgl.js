
var RSGL = {};

RSGL.GOLDEN_RATIO = 1.618;
RSGL.TAU = 2*Math.PI;

RSGL.DEFAULT_COMPASS = {
  body : {
    inner_diameter : 10,
    inner_thickness : 1,
    rim_diameter : 12,
    rim_thickness : 1,
    color : 0xcccccc,
  },
  
  needle : {
    needle_width : 1,
    needle_length : 9,
    color : 0x993333,
  }
};

RSGL.DEFAULT_HEALTH_BAR = {
  width : 20,
  height : 5,
  color : 0x993333,
  line_width : 3
}

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

  var animated_needle = new THREE.Object3D();
  animated_needle.add(compass_needle);

  var compass_group = new THREE.Object3D();
  compass_group.add( compass_body );
  compass_group.add( animated_needle );

  return function(environment) {
    animated_needle.applyMatrix(new THREE.Matrix4().makeRotationZ( -RSGL.facingToRadians( environment.payload.compass ) ) );
    
    return compass_group;
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

/**
 * Returns a rectangular geometry, centered at the origin, with the specified width and height.
 *
 * This geometry will be suitable for the Line*Materials.
 */
RSGL.createOutlineRectangle = function( width, height ) {
  var outline = new THREE.Geometry();
  outline.vertices.push( new THREE.Vector3( -width/2, -height/2, 0 ) );
  outline.vertices.push( new THREE.Vector3(  width/2, -height/2, 0 ) );
  outline.vertices.push( new THREE.Vector3(  width/2,  height/2, 0 ) );
  outline.vertices.push( new THREE.Vector3( -width/2,  height/2, 0 ) );
  outline.vertices.push( new THREE.Vector3( -width/2, -height/2, 0 ) );
  outline.computeLineDistances();
  
  return outline;
}

/**
 * Returns a rectangular geometry, centered at the origin, with the specified width and height.
 *
 * This geometry will be suitable for the Mesh*Materials. At the time of this writing I do
 * not understand why geometries need to be built in a different way for line -vs- mesh materials.
 */
RSGL.createFilledRectangle = function( width, height ) {
  var shape = new THREE.Shape();
  shape.moveTo( -width/2, -height/2 );
  shape.lineTo( width/2, -height/2 );
  shape.lineTo( width/2, height/2 );
  shape.lineTo( -width/2, height/2 );
  shape.lineTo( -width/2, -height/2 );
  
  return new THREE.ShapeGeometry( shape );
}

RSGL.createHealthBar = function( health_bar_description, fraction_filled ) {
  var width = health_bar_description.width;
  var height = health_bar_description.height;
  var color = health_bar_description.color;
  var line_width = health_bar_description.line_width;
  
  var outline = RSGL.createOutlineRectangle( width, height );
  var fill = RSGL.createFilledRectangle( width, height );
  
  var outline_mesh = new THREE.Line( outline, new THREE.LineDashedMaterial( { color : color, linewidth : line_width, dashSize : 0.5, gapSize : 0.5 } ) );
  var fill_mesh = new THREE.Mesh( fill, new THREE.MeshBasicMaterial( { color : color } ) );

  var scene = new THREE.Object3D();
  scene.add( outline_mesh );
  scene.add( fill_mesh );

  return function( environment ) {
    var percent_health = environment.payload.health["absolute-health"] / environment.payload.health["max-health"];
    
    fill_mesh.position.x = width / 2 * (1.0-percent_health);
    fill_mesh.scale.x = percent_health;
    fill_mesh.updateMatrix();
  
    return scene;
  }
}

RSGL.createStatusBlock = function() {
  var compass = RSGL.createCompass( RSGL.DEFAULT_COMPASS );
  var health_bar = RSGL.createHealthBar( RSGL.DEFAULT_HEALTH_BAR );
  
  return function(environment) {
    var status_block = new THREE.Object3D();
  
    var animated_compass = compass(environment);
    var animated_health_bar = health_bar(environment);
    
    animated_compass.position.x = 10;
    animated_health_bar.position.x = -14;
    
    status_block.add(animated_compass);
    status_block.add(animated_health_bar);
    
    return status_block;
  }
}

RSGL.createCamera = function(width, height) {
  var camera_world_width = 25;
  var camera = new THREE.OrthographicCamera(
    -camera_world_width,
    camera_world_width,
    camera_world_width/width*height,
    -camera_world_width/width*height,
    1,
    100 );

  camera.position.set( 0, 20, 20/RSGL.GOLDEN_RATIO );
  camera.up.set( 0, 0, 1 );
  
  return function(environment) {
    camera.lookAt( new THREE.Vector3(0,0,0) );
    return camera;
  }
}

RSGL.createLighting = function() {
  var sunlight = new THREE.SpotLight( 0xAAAA88 ); 
  sunlight.position.set( -15, 0, 15 );

  var skylight = new THREE.HemisphereLight( 0x666688, 0x663300 );
  skylight.position.set(0,0,100);
  
  var light_group = new THREE.Object3D();
  light_group.add(sunlight);
  light_group.add(skylight);
  
  return function( environment ) {
    return light_group;
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
  
  scene.add( RSGL.createLighting()(environment) );
  scene.add( RSGL.createStatusBlock()(environment) );

  renderer.render( scene, RSGL.createCamera(width, height)(environment) );
}

$('.webgl-container').each(function(i,container){ RSGL.initialize($(container)) });
