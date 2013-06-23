Putting it all Together: building a full featured web application using pure Haskell
====================================================================================

There are a lot of small tutorials out there that show individual components of how to build a Snap Web Application. However, there really isn't a complete example that shows how to put them all together in a well formatted way. 

This boilerplate project aims to help new users quickly jump into making pure Haskell web applications. All the components are separated so expanding them into a more complicated setup should be relatively straight forward.

At a glance
-----------

This Boilerplate provides many things which should provide a good base for anyone to quickly get started.

* Snap base
* OpenId
* Digestive Functors
* Heist Templates
* IxSet for Multiple Key Based data structures
* Acid State for persistence
* Snap Sessions used for OpenId Login
* Standard MVC format

What this boiler plate project includes
---------------------------------------

    Main.hs
> Houses our server code. This is basic Snap and should be nothing new.
> Nests our Applets (Maybe this should go in SnapApp/Application?)

    SnapApp/Application.hs
> This is our main Snap App DataType. 
> We have a seperate file so we don't have circular imports

    SnapApp/Utils.hs
> A few functions to help us render Blaze Templates in Snap

    SnapApp/Routes.hs
> All our routes that our application uses.

    SnapApp/Views.hs
> This is the bread and butter of our web app. Here we provide the OpenId Login information. A basic view to process something using digestive Functors, and an example of Snap Sessions. To implement OpenId, this boilerplate uses the [Authenticate](http://hackage.haskell.org/package/authenticate) library. For more information on Digistive Functors, please take a look [at the Github Repo](https://github.com/jaspervdj/digestive-functors/). The digestive functors view example just validates a form. It really doesn't do anything with the data (e.g. register a user), so don't look for it to do anything meaningful, you'll have to do that yourself.

    SnapApp/Forms.hs
> An example of how to use Digestive Functors. Essentially this was taken from the [digestive happstack example](https://github.com/jaspervdj/digestive-functors/blob/master/examples/tutorial.lhs)

    SnapApp/Models.hs
> Our models. You can use any native Haskell datatypes here, however in this case we use IxSet in order to filter on mutliple keys. For more information of IxSet look at the [HappStack Tutorial](http://happstack.com/docs/crashcourse/AcidState.html). Even though it is for Happstack, this boilerplate should provide you with enough context to migrate things over.

    SnapApp/Controllers.hs
>This is a mix of IxSet and AcidState. It uses IxSet to query our users, and uses AcidState to keep things persistent. See the [HappStack Tutorial](http://happstack.com/docs/crashcourse/AcidState.html) for more information.