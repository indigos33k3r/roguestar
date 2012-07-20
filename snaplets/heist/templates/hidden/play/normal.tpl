<apply template="/hidden/context">

<div id="magicbox">

<div id="gameplaybox" class="roguebox">
  <statsblock/>
  <map/>
</div>

<div id="messagebox" class="roguebox">
  <messages/>
</div>

<div id="controls" class="roguebox">
<form action="/play/move" method="post">

<div>
<select name="mode">
<option value="normal" selected="selected">Normal</option>
<!-- 
<option value="attack">Attack</option>
<option value="fire">Fire</option>
-->
<option value="jump">Teleport</option>
</select>
</div>

<div>
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
</div>

</form>

<!--
<form action="/play/inventory" method="get">
<button type="submit">Inventory</button>
</form>
-->
</div>

</div>

</apply>
