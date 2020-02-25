package ipoemi.zio.study.overview._02_creation_effect

object Legacy {
  def login(onSuccess: User => Unit, onFailure: AuthError => Unit): Unit = {
    onSuccess(User("1"))
  }
}
