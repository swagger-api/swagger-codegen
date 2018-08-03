package io.swagger.v3.generator.model;

import java.util.Collections;
import java.util.List;

public class HiddenOptions {
    private List<String> clients = Collections.EMPTY_LIST;
    private List<String> servers = Collections.EMPTY_LIST;
    private List<String> clientsV3 = Collections.EMPTY_LIST;
    private List<String> serversV3 = Collections.EMPTY_LIST;

    public static HiddenOptions getEmpty() {
        final HiddenOptions empty = new HiddenOptions();
        empty.setClients(Collections.EMPTY_LIST);
        empty.setServers(Collections.EMPTY_LIST);
        empty.setClientsV3(Collections.EMPTY_LIST);
        empty.setServersV3(Collections.EMPTY_LIST);
        return empty;
    }

    public void setClients(List<String> clients) {
        this.clients = clients;
    }

    public void setServers(List<String> servers) {
        this.servers = servers;
    }

    public boolean isHiddenClient(String client) {
        return clients == null ? false : clients.contains(client);
    }

    public boolean isHiddenServer(String server) {
        return servers == null ? false : servers.contains(server);
    }

    public String clients() {
        return clients.toString();
    }

    public String servers() {
        return servers.toString();
    }

    public void setClientsV3(List<String> clientsV3) {
        this.clientsV3 = clientsV3;
    }

    public void setServersV3(List<String> serversV3) {
        this.serversV3 = serversV3;
    }

    public boolean isHiddenClientV3(String clientV3) {
        return clientsV3 == null ? false : clientsV3.contains(clientV3);
    }

    public boolean isHiddenServerV3(String serverV3) {
        return serversV3 == null ? false : serversV3.contains(serverV3);
    }

    public String clientsV3() {
        return clientsV3.toString();
    }

    public String serversV3() {
        return serversV3.toString();
    }
}