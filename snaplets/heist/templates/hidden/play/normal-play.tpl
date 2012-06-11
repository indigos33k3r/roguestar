<apply template="/hidden/play/context">
<div class="gameplaybox">
  <statsblock/>
  <map/>
  </div>
<form action="/play/move" method="post">
<div>
<input type="radio" name="mode" value="step" checked="checked"/> Walk |
<input type="radio" name="mode" value="attack"/> Attack |
<input type="radio" name="mode" value="fire"/> Fire |
<input type="radio" name="mode" value="jump"/> Jump |
<input type="radio" name="mode" value="turn"/> Turn In Place
</div>
<div class="buttonrow">
<button type="submit" name="direction" value="nw">NorthWest</button>
<button type="submit" name="direction" value="n">North</button>
<button type="submit" name="direction" value="ne">NorthEast</button>
</div>
<div class="buttonrow">
<button type="submit" name="direction" value="w">West</button>
<button type="submit" name="direction" value="wait">Wait</button>
<button type="submit" name="direction" value="e">East</button>
</div>
<div class="buttonrow">
<button type="submit" name="direction" value="sw">SouthWest</button>
<button type="submit" name="direction" value="s">South</button>
<button type="submit" name="direction" value="se">SouthEast</button>
</div>
</form>
</apply>
