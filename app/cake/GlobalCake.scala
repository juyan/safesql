package cake

import safesql.DefaultMySQLClientComponent
import services.DefaultAuthServiceComponent

/**
  * Created by junyan on 3/26/16.
  */
trait GlobalCake extends DefaultAuthServiceComponent
  with DefaultMySQLClientComponent {

}

object GlobalCake extends GlobalCake
