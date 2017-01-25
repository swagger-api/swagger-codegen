# Swagger for Rails 5

This is a project to provide Swagger support inside the [Rails](http://rubyonrails.org/) framework.

## Prerequisites
You need to install ruby >= 2.2.2 and run:

```
bundle install
```

## Limitation: Pluralization
We don't have access to Rails inflections that let us do #pluralize so we have to simply add "s" to each controller. This means if you have a model called "sheep" the generated code will be sheeps (incorrect). In Rails, the pluralized version of sheep would also be, sheep.

You can see this specifically with the filename for the controller:

We want: sheep_controller.rb

We get: sheeps_controller.rb

The tables will still be correctly named since we can use the Rails inflections in that file.

## Getting started

This sample was generated with the [swagger-codegen](https://github.com/swagger-api/swagger-codegen) project.

```
bin/rake db:create db:migrate
bin/rails s
```

To list all your routes, use:

```
bin/rake routes
```
