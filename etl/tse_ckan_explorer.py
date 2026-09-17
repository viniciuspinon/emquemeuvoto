import httpx
import json
import time

def check_tse_ckan():
    client = httpx.Client(
        verify=False,
        headers={"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"}
    )
    
    # 1. Test package_search with tag or query
    for endpoint in [
        "https://dadosabertos.tse.jus.br/api/3/action/package_search?q=candidatos",
        "https://dadosabertos.tse.jus.br/api/3/action/package_search?q=2026",
        "https://dadosabertos.tse.jus.br/api/3/action/package_search?fq=tags:Ano%202026",
        "https://dadosabertos.tse.jus.br/api/3/action/tag_show?id=Ano%202026",
        "https://dadosabertos.tse.jus.br/api/3/action/group_package_show?id=candidatos"
    ]:
        try:
            print(f"\n--- Consultando: {endpoint} ---")
            r = client.get(endpoint, timeout=15)
            print("Status:", r.status_code)
            if r.status_code == 200:
                data = r.json()
                if "result" in data:
                    res = data["result"]
                    if isinstance(res, dict) and "results" in res:
                        print(f"Encontrados: {len(res['results'])} datasets")
                        for p in res["results"][:5]:
                            print("  - Title:", p.get("title"), "| Name:", p.get("name"))
                            for res_item in p.get("resources", [])[:3]:
                                print("    * Resource:", res_item.get("name"), "| URL:", res_item.get("url"))
                    elif isinstance(res, dict) and "packages" in res:
                        print(f"Packages no tag: {len(res['packages'])}")
                        for p in res["packages"][:5]:
                            print("  - Title:", p.get("title"), "| Name:", p.get("name"))
                    elif isinstance(res, list):
                        print(f"Lista com {len(res)} itens")
                        for p in res[:5]:
                            print("  - Item:", p.get("title") if isinstance(p, dict) else p)
            time.sleep(1)
        except Exception as e:
            print("Erro:", e)

if __name__ == "__main__":
    check_tse_ckan()
