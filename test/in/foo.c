// simple multithreaded web server

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h>

#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>

#include <unistd.h>

#define PORT 8080
#define MAX_REQUEST_SIZE 1024

void *handle_request(void *arg) {
  int client_socket = *(int *)arg;
  char request[MAX_REQUEST_SIZE];
  int request_size = read(client_socket, request, MAX_REQUEST_SIZE);
  if (request_size < 0) {
    perror("read");
    exit(1);
  }
  printf("request: %s\n", request);

  char response[] =
      "HTTP/1.1 200 OK\r\nContent-Type: "
      "text/html\r\n\r\n<html><body><h1>Hello, world!</h1></body></html>";
  int response_size = strlen(response);
  int bytes_sent = write(client_socket, response, response_size);
  if (bytes_sent < 0) {
    perror("write");
    exit(1);
  }
  printf("response: %s\n", response);

  close(client_socket);
  return NULL;
}

int main() {
  int server_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (server_socket < 0) {
    perror("socket");
    exit(1);
  }

  struct sockaddr_in server_address = {.sin_family = AF_INET,
                                       .sin_port = htons(PORT),
                                       .sin_addr.s_addr = INADDR_ANY};
  if (bind(server_socket, (struct sockaddr *)&server_address,
           sizeof(server_address)) < 0) {
    perror("bind");
    exit(1);
  }

  if (listen(server_socket, 5) < 0) {
    perror("listen");
    exit(1);
  }

  while (1) {
    struct sockaddr_in client_address;
    socklen_t client_address_size = sizeof(client_address);
    int client_socket =
        accept(server_socket, (struct sockaddr *)&client_address,
               &client_address_size);
    if (client_socket < 0) {
      perror("accept");
      exit(1);
    }

    pthread_t thread;
    if (pthread_create(&thread, NULL, handle_request, &client_socket) != 0) {
      perror("pthread_create");
      exit(1);
    }
  }

  return 0;
}
