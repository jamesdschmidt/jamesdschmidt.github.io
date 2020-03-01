---
title: "Contact"
classes: wide
permalink: "/contact/"
---

<a href="mailto:{{ author.email }}">
  <meta itemprop="email" content="{{ author.email }}" />
  <i class="fas fa-fw fa-envelope-square" aria-hidden="true"></i> {{ site.data.ui-text[site.locale].email_label | default: "Email" }}
</a>

<a href="https://github.com/{{ author.github }}" itemprop="sameAs" rel="nofollow noopener noreferrer">
  <i class="fab fa-fw fa-github" aria-hidden="true"></i> GitHub {{ author.github }}
</a>

<a href="https://www.linkedin.com/in/{{ author.linkedin }}" itemprop="sameAs" rel="nofollow noopener noreferrer">
  <i class="fab fa-fw fa-linkedin" aria-hidden="true"></i> LinkedIn
</a>

<a href="https://twitter.com/{{ author.twitter }}" itemprop="sameAs" rel="nofollow noopener noreferrer">
  <i class="fab fa-fw fa-twitter-square" aria-hidden="true"></i> Twitter @{{ author.twitter }}
</a>
