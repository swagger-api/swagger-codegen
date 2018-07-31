package io.swagger.v3.generator.model;

import java.util.Collections;
import java.util.List;

public class HiddenOptions {
    private List<String> clients;
    private List<String> servers;

    public static HiddenOptions getEmpty() {
        final HiddenOptions empty = new HiddenOptions();
        empty.setClients(Collections.EMPTY_LIST);
        empty.setServers(Collections.EMPTY_LIST);
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
}