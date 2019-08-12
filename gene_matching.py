import pandas as pd

example_gene='tcaggcacctaatgccatcgctaattaccacaaatatttccgaggacacgcttccaacgctatctaaaagccgacctgggggcaggcaatagtcagtcacgattagcggccgaggcctatttactgggaggcacagtcgatgcagtgttcttatacaaactctggcctgtgagcggttagggtaggagcgtatacggtagcagaatggttacccgctgtcgtagttgcgggtgggaggccacactgagagatcggtttgctcgagtctgtacaaaaatcacgcagtcaacgtctttcgttccctgcccctggagttcttcctgttttctagacggtagacttctctagtaaacaacggacgctcacataccggtccgccgccatgtagtcacgaacacgtaacgatcaggcccctctcatggaggccggcgatgaacagaacacatacgaaatacctgagccgtaaagtaatctacgccattgctgaccgccccggaatcacggtcggttctaggggattacgtcgttttgttatcttctcacgcacggaagccatgtacctgttgttgaagtatgtagtcgttgtatcccacagagactggatctccatctcgcgcatcctaaccccggtacttagctcattacgcttcgtataatgaccggacgagagctcgtcttatcacctcaatgagagacccactcagttgaccagaacccgagtaaattcagaccctatggcaaggtacacgtggccgctttattggccgcgcttttagagatacgccaagggactgatgggagttggtttgataacgttagttcatgttaaatttaggttctaacatctcattgttgagatgaggcacaggaaggtggagcacatcagtttgttactgtgatctgtagtaccaggatgtacccgtatggggcacattccgaatacctcgccatgttgtgtacgtttggtccggtcattgctccaaccctgcattaaataagtgcagacgtagctcgttcttcggtctagcaatcgtctctgtaatatctctgtggataacgtgttgtgtagtgcgattatcgatggcgcggggtccgtgggcaaaatcgcggtatactgaacgccgtctgtttagttctgtctacttgatgttcaagatccggctcatcggagcccaagtacaagaaggggtaaccatcgctttgtcatagcacaactctggtacaaccacgcggctaactctcctaggggctaggcaaacgatgtcgcattcacgcatggtatctcccggcaggaaatctaatcaacgtccctagcccagctctgcacaaagacccccgtacaactgacgaggcgcagcgcccaccactgcaccgactgaccgcaaggactccccagctacaagtgacgccatcctggattgtccgtccgaacgcgccctggccagtaccgtgattttgggaccggttatctcttttcggcaggagatgtatcccctggcggggccggagtttgctttgcgacgtttaccacgcatactcagggcgctaatcacatgtacgctgcttaccttcccggccagcgacttcacattttggccaccttatctttcggcgagaggccatctcagatatctctttcctgacataagcactaattatcacagggtcctaaaagtgaactccgtcgatgctcgggcgtctgaaaagcgttcgattaccggcgtagtgagaaccctcggtcgccgagtgtaacgtatagtctagaacccgagcaagtatgatcgtccaagagtatacccagaatactcgcaattaaacccctgaccatgtgatatcatactgaacagccttggtcgttaaggacaaggcgcctcgttatcgatttacttccatgatcgtccccacatgtccagctcgagcttttccgtgaagtggcgtcgccggcagagctgccatcagggggcgtgtacgtgaaataccagcccaacagcgcattaaggatcgatcttgagggatggatgttgcttcctacatgatcttcgaaccaatatccaggacccagggtcctagtttgtggtgcttatggatcacgggtgacaaacccgcgacttagtgcttaaacaaacaaacggaaagtatcgttcatgggcggcgcaagggaaactctgtccaatatctggtccgtaggtcggccgaggcaatgggcccgaggagggaagtatattaggccgtgtccttgcaagcgcggccttgggctatagccttgtgcaaagtcagtcttgtccgggtctccccacagtaatcgttctgtagatggggtacgtccttttacagggaaacctggaccgcgcgttcgctggaccgagaggtctaaggagagcaatgattgttacagcctgccctctcaaggatatattgtcggtcgtcctgtaagccattcagttccgtctacaattcactgttaagtgatcagcctcgtaaatcgactctgtctttctgaattatgtcggtggtactaggggagtcggctgtaccgtcagctatgactcggatgttctgtgagtgcccttttagactcctaaagcctaatcagcacggtggacgtaacagtgcgactacaaagatatgctaaggtagcttggaatcctgatgggtggccacggacgacgaactgcgacggcctcgtttcattatgtatccggctatgggacggataagatgtactaacagtcgccggctcaagcagctaaccgattgtgttgtaagtgtgccaacataaactcagggcgatgaattcactgatacgaccttgcatcgaaggtttcctttaatgtggcgagaggccggcgccctgtactagtaccaggggaggagaatgaaaacgtcggcaccaggaacggacgtatagggagtttcatgttcctttgatgatcttcgtcagtaccatcgagacggttggtcctgagcaccgacgtccatgttcgacaatgctgctatcataattcgtctagactgcgacacgccaccgatttgaaaaaactatgcagacttagggcggcgccaccctgtagtgtcggtgaagcatgcagtcacaggcaaggttgaggtagcgtcttcgacagaatttcggcgctctttaacgtcggtcttggccacggtatgaatttggcacccacaaacggatagtgataacattatgttttcagtaagtgagcgctcttacctcgcaggtgacgcaacagctaagagttttttctgcttactagctgcaaatcatttggttcgcaccgtctgacttccattcacacagcctcaaggtgggggaacttcgtgtaaaaagtatggaatacagtggtcgaggccgcagagttaagactagtcttacgttacgacgacgggggaataaggcaaccgctaccatacgtacgccaacgcacattcagtactatgtatgcacccaccgaaagggttatactcgatccgcaaggtggttacacgtgattcaacggccggaattttaatcgaacttattttaatccactaggtaggatggatcgctggactcgaatagacttgtgtgacaatagacgcgcacgagacgtgacggtcccctagaagtatttgagttcagtgtagagaacgcatagcatgccgcctcccagactctcagacccgcgaccggagataaattctttgagtgcatggcccaggtagcgacttatacgacttttacccacgtgatcgcaccatccgtgtggcctcaggagatcacatggatccgggagaaaagattgattgaattcgaaatgctaaaaacctcttccgtcagggacttcccgacaacaagggcgctcggacaccaaagccgaacgcaagtccttgcgtactctaatccgccacactgaatggagcggcggtaaataaaatttgatcgtaggaggcgtgtaataaggctgtgcttagacttgcagagatcacaccaagtcgcgcaattcccttatccacgtgggtgcgtacctcggaggatctgatactagttgcctcagtgagacgcagatagcgacgaacactgaagcggaatattacagcaggactagggacgctatatcgggttaccttatgccccctgaaccaagctaactatggatcggctcaggcttggattgcttgtcagcagtgcagtgggatttgagctatgatccgtacggacacaatttgagacttatcaatcgaatcctatacaaacgtcggtatccgcttagcgcgtagggtgaactcaatcgtgggatttcctgcaaaaggggcggcgttgctaactcgccgacattccccgggtattaggactatgaagaacagcgtaatcactgcactgttcggacaattaaccaaagcgtattactcggtcatctagtaccttttataacccggttgcgcccaagttcattagccgaccggaggtttctgctacattcgttgtagcatcgtactccgctagagcacagcaagccctcacccgtagtgaactggctgctcggaatcaaagaatgtgtcccgtgcttttactcctgctccacagtattagaatctaaggtccgaacttaatctagtaaccatccgtgaaatggggtatattccaaacatggttaatacgcaggggcaggaagcgcttgcgatgattctcccaggcgcctcggcgcgttcgcaggaacctaactcggaccccgtccggactgacagcaccgcccaaatatagtgtccgtgcctcatccacagccgccccacggaatttagctcaataatatttatgcttggcctcaggtctaattggtatcatgcggggctcggtattcgggagatgacggtgcatgctggtacttctggacataactttgttgacggtttttgctgctattgcaggcctacccttagtcaacgagggcgcgtatctcgtcgtaccatgtggtaactgatacgcacaattagtggacctatgataacaacccccttccttcaatggagggaatttcgttctacaataatctgccataggtacgctactgggagcttgtcaggcttgaggccccagtttaggtctaagtttcagcagtcctcgaccgtagggatctttctgaggcgttaccccataactcggcagactgcccattccgattaaaacgcagaagcccgcgcgctcctcactttgcggtggcaaggcagcgggaacgacaccccctacatgctagatggctgccaggccctagatcccaatattgttgctcatcacggcacctgaaatacctgaaaataccatatttagttaagtcggattgtatttctactgcattcaggaggtggccgaagaaatgggttatatcatatgaatgtttattaagggttgcgcatttgatccacctctagggcggtggctaaattgaggcccagcatggattacagtgcggagtcgagtgtgtcgcaaaaattggtcttgtacttgagaagactcaagcataaaggattcgggaaagtggtttgccggtaagaccctttgcgacgattgttatcacagtcacagagtagtatccaattcgcggcagctcttcgtaaatgtggtggacttgtgttcacagaatagaagtgtattgagcaaataacgggcagatcgggacgctatagaatattttgcattgggctaggcaatgcttcggggcaagccgttcgtccgctggcatctaactagagcgcagtcgaaacgggagagcatcctgactccgtcgtttttgttattacactccacgtgcctacacttatcgcacaacaagcagattcgatgcaggatcttcttatacgggcagcattcgcgtacagtcagaaagcaggccccaagcgttacaaacgtgggttcgacattgttgcggtattgcgcaaaggattgatagtgaccggggtcaacgtccatttggtaaagcagatgtttagcgtatatccacccagtaatcgcgaactttttttatgcacgtcggctgtattatgcacggaacgatcggacgagccccttgccccagaatgtcagctgttatccaccttagggagtctaacggggcatcgttggggtaacgctcagacgctgcggttcgatatggctcactagtggtcagcgagcgcgaaaggttcttgccttaaaagtgcttttgtgccttctcctgccaggaaggacctcagcttagaatctggtacgaaaaccctagctccatagggacctgaagacgtcaattacgttggtccctcgctcaaaactagtatactaacggcaagagtgtcggggacgatcttagtggagaaccgttggagggcttcagggaagtagaatgaaggttacttaagatctgtattggcgaacatcgttgtgcccttcagcaaaggtagatttcctaaaaagagtgtagcgttaccgaggcgtctgctcgttgaagcttacagtcctacaatagacagaatatgccgaggagcacgcgcattaacgacacccgaatatcaattccccctcagaccgtgacgcggctctcttcccgttcagtcacgtgtgcttgaaacgcatctgcaaattcgggtcgcatacttcatcacatacttttaatccaacctaccgatagactctttgagtgttatatgtagttaccatctaatgtactaggtatatacccaccgattgcctgtcggtaggtcaatggatctgttggtcctcagtaggatttctaagtttgggctgctcctctttctataatgagaagccgtattttctaatggagagaattcgcgggttagctcccgagacaacggtacatttatttcgaacgacggggttctgatcatcaatcgatcacgctatacgtcggcagagacacggccggtatatctgctcttaatcttaatggtccaaagctgcgtgtcttgcagtgtctttattcgtcatcatgtctactcaaagccctgggtttctctcaacgaccacaaaagtgacggctctgcagagtatttacgtacgacgctagctgcatccattttttgtaggacccggcgctgcgaagacggtgtcgccggttcgtcacttagccttaccatgagtagagctgagacggcttccgatagactaagcaaagctgcgtaaccacacggcggcggctccgttccccggccatgccacagtttatagctgaagctacacgtgcaacctcacttaaacgccagccgcacaagctctgaaatgctaggtatacagggaactagtgactgtatcacgtttagaatcccaatagaggactcggaagaagataagtatcggtttcgaatgaccctaggacgacttctttcaattattttgattaccttcagtcgacgatcccatttgcatgccgcttgatttcgcccaatcggtagtttacaccttacggaatttcatgacctggtacatagttccttgcgaaccaaatttttttgatcgaatgtaagcggttcattctgtggttgtgagataaatgtaaccatgacgtccctaagaaatggagacataagggtctcccttctcgcatttcactctcctggtcatgttaacgcgcggcgggatgtactgctgtcttgctaggctcttccaatcgaaagccatttgcataattcttggatttgactacgtttgatgcagtccctcccacttcaccaataccctgttacccttactctcccacctatagtaaaaggtagaaataagcatggcgctatttaagtgtgacacctgtgcatgcgtgaatggactcgtacaggacaggggcgctagagaggactggggctacttgtaggcgacggcttcccgcgctatctgatcaagggatcaggaacaactgcctgtacttttttgggatatcgtcaacgcagttactatcatccaagctagactaacttcggatacggacagttcttcccgtctgacttgtgagttacggggggatgcgtctccgctcacttacggttactcggttgactctacaagaagcgacgataggactgcgccttttctcctggggactaggcaggttttgcttgccaatcccatattgtgtgaagtggtagaccagagtgcaatacaccggccctgactcagagtcagattcatgctcgccggcgcatatgtcatctcccctgccccctgttggagaccgggaatcggtaaaaaggttgttgccatggttgggtcgcccttccattaccacgagtaagtttcaccctgtcagaccgaacaataactcgactgagaggcatcggtatcatggtccggctgagaggcgtgcaaacactaagacgctcgttgtcggacggatgccctatagtattaccagtctattgccgggcaagctgttgagtcatgtctagccaccttcctaataaccgaggacagcgctagagataaccacgctgcgcagtttgcgcagtcctgcctggaggaccttcaaatatagtttcgcctacgaatagcaccgagacggagctacgtgagtcctggagtcgatcagagattatttcggcgtgcccccccccgggaacggcgctgcgacggaatcaaacgagtccaaaatagttgttgtaaccatgctgctttctcagaatttgccctcattcgcgctacttagcataccatgtcaaaagcggtaaggccgttttgggtgtgcacataaatctacgtcaatcgtaaacctgattgcggggaacacggtcagttacaacacgagtcttctcagtgagaattagtgtttctacacgctccgatgccattaacacgattgcactcttttacgggaaatttggtgcgagacatcgcttccaatttatactgctggtagcaatgcacgactgcggcaaggtctaggatctcaagatacccgattagagtgcccccggcccgtgcgttgcgatatatcatccatgaggtcctgcggtatcttgtacggccagggggttcgagaactccaacgatactcaaaagatgccggatcagacgggataagattagactaaactcaaaaccacaccctttgatgttaggagaacgattaccatgataattctatcaagggctttgccggaacactgacctaacatgtgtacttgaccgatacgttatactttcacagagtcgctgaccccagcgcttagttatatttcaattggattccccccctaatgtccttagacaactggataaatgcctctaaccgactgcgattagtctactttatatcttgttgcggcatacggttttgacaactacacgcaagtgtgatgacgcaatcatcaatagagtacccgtgaacttagattacggatttgtcggtaacgggtcgttcaaaaacgtctaccggacggtcgactctccgtagaaaacgttacagcagcttccatgtcgcctgaacggctgtaatgtacaaacgaacgaaaagcaagcagctcatggtagacagtgcaaagggcatacttaagtttttacctgttcgtgaacaacgatgctccggctccactaggatggtcaagcacacctagtaaccgttgctatcgatgaaccaccccagtaaacccaggtagccggttcagcgttagcggacctggagcattcacgggcagcatcccgcgggttagcacgttcgaaatgtccgttatccggttggattacgtgtagtaggcagtgtctatgctctcccatgcggattgttctggtatcttatacgactgtaaagcgtgtctatggacatggaatataaaaatacctggatatcacaaatcaataattgagtagctggtaagatcacaccttagacacttgatctaaatggttcatctatatctgaaccgatgaagtgtttccaggttgttttgacatgtctttacaaattacgagttgtgccccgtcgcgatagcagtcggtcaggctccttcgccgctctcacccggcggatta'
example_search='cgtgcctggttcgttcggaa'


# Excact match
def exact_matches(gene,search_str):
    matches=[]
    search_len=len(search_str)
    for i in range(0,len(gene)-search_len):
        if gene[i:search_len+i]==search_str:
            matches.append(i)
    return matches

def _percentage_calculation(str1,str2):
    correct=0
    for i in range(0,len(str1)):
        if str1[i]==str2[i]:
            correct=correct+1
    return correct/len(str1)

def percentage_calculation(gene,search_str,top=10):
    power={}
    search_len=len(search_str)
    for i in range(0,len(gene)-search_len):
        power[i]=_percentage_calculation(gene[i:search_len+i],search_str)
    return pd.Series(power).sort_values(ascending=False)[:top]

# Weighting the first initial character weighted higher
def _weighted_percentage_calculation(str1,str2):
    correct=0
    total=len(str1)*(len(str1)-1)/2
    for i in range(0,len(str1)):
        if str1[i]==str2[i]:
            correct=correct+(len(str1)-i)/total
    return correct/len(str1)

# Inverse linear weighting
def weighted_calculation(gene,search_str,top=10):
    power={}
    search_len=len(search_str)
    for i in range(0,len(gene)-search_len):
        power[i]=_weighted_percentage_calculation(gene[i:search_len+i],search_str)
    return pd.Series(power).sort_values(ascending=False)[:top]



