---
layout: page
title: Archive
permalink: /archive/
---

<div class="center-list">
<ul>
{% for post in site.posts %}

<li><a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a></li>

{% endfor %}
</ul>
</div>
