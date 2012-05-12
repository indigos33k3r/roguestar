<apply template="/hidden/context">
<h1>Help!</h1>

<apply template="/hidden/ui/faq">
  <apply template="/hidden/ui/faqbox">
  <bind tag="question">What the hell is this?</bind>
  <bind tag="answer"><apply template="/hidden/help/wth"/></bind>
  </apply>
</apply>

<apply template="/hidden/ui/faq">
  <apply template="/hidden/ui/faqbox">
  <bind tag="question">Keyboard Commands</bind>
  <bind tag="answer"><apply template="/hidden/help/keys"/></bind>
  </apply>
</apply>
</apply>
