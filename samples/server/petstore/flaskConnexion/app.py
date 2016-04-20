#!/usr/bin/env python3

import connexion

if __name__ == '__main__':
    app = connexion.App(__name__, specification_dir='./swagger/')
<<<<<<< HEAD
    app.add_api('swagger.yaml', arguments={'title': 'This is a sample server Petstore server.  You can find out more about Swagger at &lt;a href&#x3D;\&quot;http://swagger.io\&quot;&gt;http://swagger.io&lt;/a&gt; or on irc.freenode.net, #swagger.  For this sample, you can use the api key \&quot;special-key\&quot; to test the authorization filters'})
=======
    app.add_api('swagger.yaml', arguments={'title': 'This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key &#x60;special-key&#x60; to test the authorization filters.'})
>>>>>>> upstream/master
    app.run(port=8080)
