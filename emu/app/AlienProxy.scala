import java.net.http.{HttpRequest, HttpResponse, HttpClient}
import java.net.{URI, HttpURLConnection}

object AlienProxy {
  lazy val apiKey = sys.env("ICFPC_API_KEY")
  lazy val endpoint = "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=" + apiKey

  def send(data: String): String = {
    val request = HttpRequest.newBuilder
      .uri(URI.create(endpoint))
      .version(HttpClient.Version.HTTP_1_1)
      .POST(HttpRequest.BodyPublishers.ofString(data))
      .build
    val response = HttpClient.newHttpClient
      .send(request, HttpResponse.BodyHandlers.ofString)
    val status = response.statusCode
    if (status != HttpURLConnection.HTTP_OK) {
      throw new RuntimeException(s"HTTP status ${response.statusCode}: ${response.body}")
    }
    response.body
  }
}
