package main

import (
	sw "./swagger"
	"context"
	"fmt"
	"log"
)

func main() {
	log.Printf("Main started!!!")

	AddPetCall()
	GetRandomPetCall()
}

func AddPetCall() {
	cfg := sw.NewConfiguration()
	cfg.BasePath = "http://localhost:8080/v3"

	fmt.Println("cfg.BasePath: " + cfg.BasePath)
	client := sw.NewAPIClient(cfg)
	newPet := (sw.Pet{Id: 12830, Name: "gopher", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending", Tags: []sw.Tag{sw.Tag{Id: 1, Name: "tag2"}}})
	r, err := client.PetApi.AddPet(context.Background(), newPet)


	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(r.StatusCode)
}

func GetRandomPetCall() {
	cfg := sw.NewConfiguration()
	cfg.BasePath = "http://localhost:8080/v3"

	ctx := context.Background()
	ctx.Value(sw.BasicAuth {
		UserName: "test",
		Password: "abc123",
	})

	auth := context.WithValue(context.Background(), sw.ContextBasicAuth, sw.BasicAuth{
		UserName: "username",
		Password: "password",
	})

	client := sw.NewAPIClient(cfg)
	s, r, err := client.PetApi.GetRandomPet(auth)

	fmt.Println(r.StatusCode)
	fmt.Println(s)
	fmt.Println(err)
}
